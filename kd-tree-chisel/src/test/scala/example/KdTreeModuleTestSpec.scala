package example

import chisel3._
import chisel3.stage.PrintFullStackTraceAnnotation
import chiseltest._
import firrtl.AnnotationSeq
import org.json4s.DefaultFormats
import org.json4s.JsonAST.JObject
import org.json4s.native.JsonMethods
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{File, FileFilter}
import scala.util.Random

trait KdTreeModuleTestSpec extends AnyFlatSpec with ChiselScalatestTester { //this: AnyFlatSpec with ChiselScalatestTester =>
  protected val jsonTestFileRegex = raw"^test\d*_d(\d+)_l(\d+)x(\d+).[jJ][sS][oO][nN]"

  protected def getJsonTestFiles: Seq[File] = {
    new File("json_tests").listFiles(new FileFilter {
      override def accept(file: File): Boolean = {
        file.isFile &&
          file.getName.matches(jsonTestFileRegex)
      }
    })
  }

  protected def commonOps(module: => KdTreeModule, useVerilator: Boolean, outputVerilogFile: Option[String], outputVcd: Boolean): AnnotationSeq = {
    outputVerilogFile match {
      case Some(filename) => WriteFile(filename, GetVerilog.processed(module))
      case None =>
    }
    val annotations = (if (useVerilator) Seq(VerilatorBackendAnnotation) else Seq()) ++
      (if (outputVcd) Seq(WriteVcdAnnotation) else Seq())
    annotations
  }

  protected def performTest(module: => KdTreeModule, annotations: AnnotationSeq, queryPoints: Seq[IndexedSeq[Int]], expectedOutPoints: Seq[IndexedSeq[Int]]): Int = {
    var elapsedClockCycles: Int = 0
    test(module).withAnnotations(annotations) { dut =>
      dut.io.in.initSource().setSourceClock(dut.clock)
      dut.io.out.initSink().setSinkClock(dut.clock)
      dut.clock.setTimeout(10000)
      dut.io.out.ready.poke(true.B)

      Seq(queryPoints, expectedOutPoints).transpose.foreach { case Seq(inPoint, expectedOutPoint) =>
        //        println(s"numNodes = $numNodes; Points: $treePoints")
        inPoint.indices.foreach { i => dut.io.in.bits(i).poke(inPoint(i).S(dut.pointElemWidth.W)) }
        dut.io.in.valid.poke(true.B)
        dut.clock.step(1)
        elapsedClockCycles += 1
        dut.io.in.valid.poke(false.B)
        while (dut.io.out.valid.peek().litValue == 0) {
          dut.clock.step(1)
          elapsedClockCycles += 1
        }
        val outPoint = dut.io.out.bits.map(sig => sig.peek().litValue.toInt)
        //        println(inPoint, outPoint.mkString("(", ", ", ")"), expectedOutPoint.mkString("(", ", ", ")"))
        //        println(s"Distances: ${KdTreeT.distSquared[IndexedSeq[Int], Int](inPoint, outPoint)}, ${KdTreeT.distSquared[IndexedSeq[Int], Int](inPoint, expectedOutPoint)}")
        assertResult(KdTreeT.distSquared[IndexedSeq[Int], Int](inPoint, expectedOutPoint)) {
          KdTreeT.distSquared[IndexedSeq[Int], Int](inPoint, outPoint)
        }
      }
    }
    elapsedClockCycles
  }

  def testWithGoldenModel(numNodes: Int, treePoints: Seq[IndexedSeq[Int]], queryPoints: Seq[IndexedSeq[Int]], pointElemWidth: Int, pRomAddressWidth: Int, useVerilator: Boolean, outputVerilogFile: Option[String], outputVcd: Boolean): Unit = {
    val kdTree = KdTree[IndexedSeq[Int], Int](numNodes, treePoints, bruteforceNodes = false)
    val expectedOutPoints = queryPoints.map { p => kdTree.findClosest(p) }

    val baseName = s"testWithGoldenModel_s${pointElemWidth}_a${pRomAddressWidth}_n${numNodes}_p${treePoints.length}_q${queryPoints.length}"
    def module = KdTreeModule.fromKdTree(kdTree, pointElemWidth, pRomAddressWidth, pRomFilename = s"pRom_$baseName.mem", treeRomFilename = s"treeRom_$baseName.mem")

    val annotations = commonOps(module, useVerilator, outputVerilogFile, outputVcd)
    performTest(module, annotations, queryPoints, expectedOutPoints)
  }

  def testWithGoldenModel(numNodes: Int, treePoints: Seq[IndexedSeq[Int]], numQueryPoints: Int, pointElemWidth: Int, pRomAddressWidth: Int, randomSeed: Option[Int] = None, useVerilator: Boolean = false, outputVerilogFile: Option[String] = None, outputVcd: Boolean = false): Unit = {
    require(treePoints.nonEmpty)
    randomSeed match {
      case Some(seed) => Random.setSeed(seed)
      case None =>
    }
    val nDim = treePoints.head.length
    val queryPoints = Generator.randomPoints(nDim, numQueryPoints, pointElemWidth)
    testWithGoldenModel(numNodes, treePoints, queryPoints, pointElemWidth, pRomAddressWidth, useVerilator, outputVerilogFile, outputVcd)
  }

  def extractJson(file: File, memoryFilePostfix: String = ""): (() => KdTreeModule, (Int, Int, Int, Int), Seq[IndexedSeq[Int]], Seq[IndexedSeq[Int]]) = {
    implicit val formats: DefaultFormats.type = DefaultFormats
    val json = ReadFile(file)

    val JObject(l) = JsonMethods.parse(json).extract[JObject]
    val map = l.toMap
    require(Seq("D", "C", "N", "P_A", "input", "output").forall(map.contains))

    val (numDimensions, pointElemWidth, treeDepth, pRomAddressWidth, treeRom, pRom) = KdTreeT.parseJson[Int](json)
    val inputs = map("input").extract[Seq[IndexedSeq[Int]]]
    val outputs = map("output").extract[Seq[IndexedSeq[Int]]]
    val baseName = s"extractJson_s${pointElemWidth}_d${numDimensions}_a${pRomAddressWidth}_dd${treeDepth}_i${inputs.length}_o${outputs.length}_p${pRom.length}_t${treeRom.length}_${memoryFilePostfix}"
    def module = new KdTreeModule(pointElemWidth, pRomAddressWidth, treeDepth, numDimensions, (treeRom, pRom), pRomFilename = s"pRom_$baseName.mem", treeRomFilename = s"treeRom_$baseName.mem")

    (module _, (numDimensions, pointElemWidth, treeDepth, pRomAddressWidth), inputs, outputs)
  }

  def testJson(file: File, useVerilator: Boolean = false, outputVerilogFile: Option[String] = None, outputVcd: Boolean = false): Unit = {
    val (module, _, inputs, expectedOutputs) = extractJson(file)

    val annotations = commonOps(module(), useVerilator, outputVerilogFile, outputVcd)
    performTest(module(), annotations, inputs, expectedOutputs)
  }

  def performJsonTests(testSpecFunc: (File, Int, Int, String, String, String) => Unit): Unit = {
    val jsonFiles = getJsonTestFiles
    for ( i <- jsonFiles.indices;
          jsonFile = jsonFiles(i) ) {
      val pattern = jsonTestFileRegex.r
      val pattern(treeDepth, numLeaves, pointsPerLeaf) = jsonFile.getName
      testSpecFunc(jsonFile, jsonFiles.length, i, treeDepth, numLeaves, pointsPerLeaf)
    }
  }
}
