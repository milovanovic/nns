package example

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import chiseltest.RawTester.test
import firrtl.annotations.{MemoryLoadFileType, MemorySynthInit}
import org.json4s.JsonAST.JObject
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.JsonMethods

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Random, Success, Try}

object WriteFile {
  def apply(filename: String, str: String): Unit = {
    Files.write(Paths.get(filename), str.getBytes(StandardCharsets.UTF_8))
  }
}

object ReadFile {
  def apply(filename: String): String = {
    val bufferedSource = Source.fromFile(filename)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }

  def apply(file: File): String = {
    val bufferedSource = Source.fromFile(file)
    val contents = bufferedSource.mkString
    bufferedSource.close
    contents
  }
}

object GetVerilog {
  def apply(dut: => Module): String = (new ChiselStage).emitVerilog(dut)

  def processed(dut: => Module): String = {
    GetVerilog(dut).split("\n").map { l =>
      if (l.contains(" * ")) s"""(* use_dsp = "yes" *) ${l.trim}"""
      else if (l.contains("] pRomMem [") || l.contains("] treeRomMem [")) s"""(* rom_style = "block" *) ${l.trim}"""
      else l
    }.mkString("\n")
  }
}

object Generator {
  def randomSigned(bitWidth: Int): Int = {
    Random.nextInt(1 << bitWidth) - (1 << (bitWidth - 1))
  }

  def randomPoints(nDim: Int, count: Int, bitWidth: Int): Seq[IndexedSeq[Int]] = {
    Seq.fill(count){ IndexedSeq.fill(nDim) { randomSigned(bitWidth) } }
  }

  def verilog(sIntBitWidth: Int, numNodes: Int, numPoints: Int, nDim: Int, pRomAddressWidth: Int = 8, postfix: String =""): Unit = {
    verilog(sIntBitWidth, numNodes, randomPoints(nDim, numPoints, sIntBitWidth), pRomAddressWidth, postfix)
  }

  def verilog(sIntBitWidth: Int, numNodes: Int, points: Seq[IndexedSeq[Int]], pRomAddressWidth: Int, postfix: String): Unit = {
    require(sIntBitWidth > 1)
    require(numNodes > 0)
    require(points.nonEmpty)
    require(points.head.nonEmpty && points.forall{ p => p.length == points.head.length }, "All points must have the same dimensionality, which must be greater than zero")
    require(pRomAddressWidth >= log2Up(points.length), "pRom address width too small to represent all points")
    val tree = KdTree[IndexedSeq[Int], Int](numNodes, points)
//    println(tree)
    val baseString = s"kdtree_s${sIntBitWidth}_n${numNodes}_p${points.length}_d${points.head.length}${if (postfix.isEmpty) postfix else s"_$postfix"}"
    val v = GetVerilog.processed(KdTreeModule.fromKdTree[IndexedSeq[Int]](tree, sIntBitWidth, pRomAddressWidth, s"${baseString}_pRomMem.mem", s"${baseString}_treeRomMem.mem"))
    val filename = s"out/$baseString.v"
    WriteFile(filename, v)
  }

  def allVerilog(sIntWitdthSeq: Seq[Int] = Seq(8, 16, 24, 32), sIntWidthDefault: Int = 16,
                 numNodesSeq: Seq[Int] = Seq(7, 15, 31, 63), numNodesDefault: Int = 31,
                 numPointsSeq: Seq[Int] = Seq(50, 100, 200), numPointsDefault: Int = 100,
                 nDimSeq: Seq[Int] = Seq(2, 3, 4, 5), nDimDefault: Int = 3,
                 pRomAddressWidth: Int = 8): Unit = {
    sIntWitdthSeq.foreach { verilog(_, numNodesDefault, numPointsDefault, nDimDefault, pRomAddressWidth) }
    numNodesSeq.filter(_ != numNodesDefault).foreach { verilog(sIntWidthDefault, _, numPointsDefault, nDimDefault, pRomAddressWidth) }
    numPointsSeq.filter(_ != numPointsDefault).foreach { verilog(sIntWidthDefault, numNodesDefault, _, nDimDefault, pRomAddressWidth) }
    nDimSeq.filter(_ != nDimDefault).foreach { verilog(sIntWidthDefault, numNodesDefault, numPointsDefault, _, pRomAddressWidth) }
  }
}

class Stack[T <: Data] (maxDepth: Int, val valueType: T) {
  require(maxDepth > 0, "Stack must have a maximum depth that's greater than zero")
  private val addrLen = log2Up(maxDepth + 1)
//  private val regs = Reg(Vec(maxDepth, valueType))
  private val regs = RegInit(VecInit(Seq.fill(maxDepth) { 0.U.asTypeOf(valueType) } ))
  val depth = RegInit(1.U(addrLen.W))

  def empty: Bool = {
    depth === 0.U
  }

  private def hasSpaceForElements(n: Int): Bool = {
    (depth - 1.U + n.U) < maxDepth.U
  }

  val top: T = {
//    chisel3.assert(!empty)
    regs(depth - 1.U)
  }

  val previousTop: T = {
    regs(depth - 2.U)
  }

  val hasPreviousTop: Bool = depth > 1.U

  def push(data: T): Unit = {
    chisel3.assert(hasSpaceForElements(1))
    regs(depth) := data
    depth := depth + 1.U
  }

  def pop(): T = {
    chisel3.assert(!empty)
    depth := depth - 1.U
    regs(depth - 1.U)
  }

  def popAndPush(data: Seq[T]): Unit = {
    chisel3.assert(!empty)
    chisel3.assert(hasSpaceForElements(data.length - 1))
    data.indices.foreach { i => regs(depth - 1.U + i.U) := data(i) }
    depth := depth - 1.U + data.length.U
  }
}

//noinspection TypeAnnotation
object TreeRomCol extends Enumeration {
  val IsLeaf = Value(0)
  val Median = Value(1)
  val PAddr = Value(2)
  val PLen = Value(3)
}

object KdTreeModuleA {
  def zFill(str: String, padding: Int, zChar: Char = '0'): String = str.reverse.padTo(padding, zChar).reverse
  def binZFill(number: Int, padding: Int): String = zFill(number.toBinaryString.takeRight(padding), padding)
}

//noinspection TypeAnnotation
abstract class KdTreeModuleA(pointElemWidth: Int, pRomAddressWidth: Int, treeDepth: Int, pointNumDimensions: Int, romInit: (Seq[(Boolean, Int, Int, Int)], Seq[Seq[Int]]), pRomMemFilename: String, treeRomMemFilename: String) extends Module {
  protected val pointElemType = SInt(pointElemWidth.W)
  protected val pointType = Vec(pointNumDimensions, pointElemType)
  protected val pRomAddressType = UInt(pRomAddressWidth.W)
  protected val treeRomAddressType = UInt(treeDepth.W)
  protected val depthType = UInt(log2Up(pointNumDimensions).W)
  protected val treeRomDataType = MixedVec(Seq(Bool(), pointElemType, pRomAddressType, pRomAddressType))

  val io = IO(new Bundle{
    val in = Flipped(Decoupled(pointType))
    val out = Decoupled(pointType)
//    val inPoint = Output(pointType)
  })

  annotate(new ChiselAnnotation {
    override def toFirrtl =
      MemorySynthInit
  })

  new File("out").mkdirs()

  protected val (treeRomData, pRomData) = romInit
//  val pRomMemFilename = "/media/konda/DCD45F82D45F5DB8/Users/Konda/Desktop/kd_tree/pygears-chisel-knn-kguni/kd-tree-chisel/out/p_rom.mem"
  WriteFile(s"out/$pRomMemFilename",
    pRomData.map(point => point.map(el => KdTreeModuleA.binZFill(el, pointElemWidth)).reverse.mkString("")).mkString("", "\n", "\n")
  )
  protected val pRomMem = SyncReadMem(pRomData.length, Bits((pointNumDimensions * pointElemWidth).W))
  loadMemoryFromFileInline(pRomMem, s"out/$pRomMemFilename", MemoryLoadFileType.Binary)

//  val treeRomMemFilename = "/media/konda/DCD45F82D45F5DB8/Users/Konda/Desktop/kd_tree/pygears-chisel-knn-kguni/kd-tree-chisel/out/tree_rom.mem"
  WriteFile(s"out/$treeRomMemFilename",
    treeRomData.map { case (isLeaf, median, pAddr, pLen) =>
      Seq(if (isLeaf) "1" else "0", KdTreeModuleA.binZFill(median, pointElemWidth), KdTreeModuleA.binZFill(pAddr, pRomAddressWidth), KdTreeModuleA.binZFill(pLen, pRomAddressWidth)).reverse.mkString("")
    }.mkString("", "\n", "\n")
  )
  protected val treeRomMem = SyncReadMem(treeRomData.length, Bits((1 + pointElemWidth + 2 * pRomAddressWidth).W))
  loadMemoryFromFileInline(treeRomMem, s"out/$treeRomMemFilename", MemoryLoadFileType.Binary)

  protected val addrOutTreeRom = Wire(treeRomAddressType)
  protected val outTreeRom = Wire(treeRomDataType)

  protected val addrOutPRom = Wire(pRomAddressType)
  protected val outPRom = Wire(pointType)

  protected val numNodes = treeRomData.length

  protected val busy = RegInit(false.B)
//  protected val started = RegInit(false.B)
  protected val validResult = RegInit(false.B)
  protected val inPoint = Reg(pointType)
//  io.inPoint := inPoint

  protected val closestPoint = RegInit(outPRom)
  protected val closestPointAssigned = Reg(Bool())
  protected val closestPointDist = RegInit(0.S)

  protected def pointDist(a: Vec[SInt], b: Vec[SInt]): SInt = {
    (a zip b) map { case (ea, eb) => RegNext((eb -& ea) * (eb -& ea)) } reduceLeft { _ +& _ }
  }

  protected def incDepth(depth: UInt): UInt = {
    if (isPow2(pointNumDimensions)) {
      depth + 1.U
    } else {
      chisel3.assert(depth < pointNumDimensions.U)
      Mux(depth === (pointNumDimensions - 1).U, 0.U, depth + 1.U)
    }
  }

  io.in.ready := !busy
  io.out.valid := validResult
  io.out.bits := closestPoint
  protected val started = busy && !RegNext(busy)

  protected def step(block: => Any): Unit = {
    when(busy) { block }.elsewhen(io.in.valid) {
//      printf("Acquiring data\n")
      inPoint := io.in.bits
      busy := true.B
//      started := true.B
      validResult := false.B
    }.otherwise {
//      printf("Waiting for data\n")
    }
  }
}

class KdTreeModule(val pointElemWidth: Int, pRomAddressWidth: Int, treeDepth: Int, val pointNumDimensions: Int, romInit: (Seq[(Boolean, Int, Int, Int)], Seq[Seq[Int]]), pRomFilename: String = "p_rom.mem", treeRomFilename: String = "tree_rom.mem") extends KdTreeModuleA(pointElemWidth, pRomAddressWidth, treeDepth, pointNumDimensions, romInit, pRomFilename, treeRomFilename) {
  private val rootStack = new Stack(treeDepth, treeRomAddressType)
  private val childStack = new Stack(treeDepth, Bool())
  private val depthStack = new Stack(treeDepth, depthType)

  private def pushStacks(nodeIdx: UInt, tryInspectSecondChild: Bool, depth: UInt, assignTreeRom: Boolean = true): Unit = {
    rootStack.push(nodeIdx)
    childStack.push(tryInspectSecondChild)
    depthStack.push(depth)
    if (assignTreeRom) {
      addrTreeRom := nodeIdx
    }
//    retrievedTreeRom := true.B

  }

  private def popStacks(): (UInt, Bool, UInt) = {
    addrTreeRom := rootStack.previousTop
//    retrievedTreeRom := true.B
    (rootStack.pop(), childStack.pop(), depthStack.pop())
  }

  private def popAndPushStacks(values: (UInt, Bool, UInt)*): Unit = {
    rootStack.popAndPush(values map {_._1})
    childStack.popAndPush(values map {_._2})
    depthStack.popAndPush(values map {_._3})
    addrTreeRom := values.head._1
//    retrievedTreeRom := true.B
  }

  private def popAndPushStacks(rootTreeIdx: UInt, tryInspectSecondChild: Bool, depth: UInt): Unit = {
    popAndPushStacks((rootTreeIdx, tryInspectSecondChild, depth))
  }

  private def stackTops: (UInt, Bool, UInt) = {
    (rootStack.top, childStack.top, depthStack.top)
  }

  protected def checkIfPointCloser(): Unit = {
    val dist = pointDist(outPRom, inPoint)
//    printf(p"Comparing points: $outPRom and $inPoint")
//    printf(p"Comparing dist=$dist, closestPointDist=$closestPointDist... ")
    when(!closestPointAssigned || (dist < closestPointDist)) {
//      printf(p"Update closest point: point = ${RegNext(outPRom).asTypeOf(pointType)}")
      closestPoint := RegNext(outPRom)
      closestPointDist := dist
      closestPointAssigned := true.B
    }
//    printf("\n")
  }

  private val pAddr = outTreeRom(TreeRomCol.PAddr.id).asTypeOf(pRomAddressType)
  private val pLen = outTreeRom(TreeRomCol.PLen.id).asTypeOf(pRomAddressType)
  private val iterPointsVisited = RegInit(0.U(pRomAddressWidth.W))
  private val calcMedianDist = RegInit(false.B)
  private val stoppedProcessingPoints = iterPointsVisited < RegNext(iterPointsVisited)

  private val addrTreeRom = Wire(UInt(treeDepth.W))
  addrTreeRom := rootStack.top
  addrOutTreeRom := RegNext(addrTreeRom)
  outTreeRom := treeRomMem(addrTreeRom).asTypeOf(treeRomDataType)

  private val addrPRom = WireDefault(
    Mux(
      stoppedProcessingPoints,
      RegNext(pAddr),
      pAddr
    ) + RegNext(iterPointsVisited))
  addrOutPRom := RegNext(addrPRom)
  outPRom := pRomMem(addrPRom).asTypeOf(pointType)

  private val closestPointDelay = RegInit(0.U(4.W))

//  println(s"pRom: $pRomData;\ntreeRom: $treeRomData;")

  step {
    when(closestPointDelay > 0.U) {
      closestPointDelay := closestPointDelay - 1.U
    }
    when (!started) {
      checkIfPointCloser()
    }

    when(!rootStack.empty) {
//      chisel3.assert(addrOutPRom >= 0.U)
      chisel3.assert(!validResult)
//      printf(p"Peek top of stack (depth ${rootStack.depth})... ")
      val (rootTreeIdx, tryInspectSecondChild, depth) = stackTops
      assert(addrOutTreeRom === rootTreeIdx, s"addrOutTreeRom = $addrOutTreeRom; rootTreeIdx = $rootTreeIdx")

//      assert(retrievedTreeRom)
//      when (!retrievedTreeRom) {
//        printf(p"Tree rom retrieval phase step (index $rootTreeIdx; $tryInspectSecondChild; $depth; $cnt)\n")
//        when(cnt === 0.U) {
//          cnt := 0.U
//          retrievedTreeRom := true.B
//        }.otherwise {
//          cnt := cnt + 1.U
//        }
//      }.otherwise {
//        retrievedTreeRom := false.B
//              printf("Point (%d, %d)... ", rootPoint(rootTreeIdx)(0), rootPoint(rootTreeIdx)(1))

//        printf(p"outTreeRom == ${outTreeRom.asTypeOf(treeRomDataType)} ($outTreeRom); addrTreeRom == $addrOutTreeRom... ")

        val nextDepth: UInt = incDepth(depth)

        val median = outTreeRom(TreeRomCol.Median.id).asTypeOf(pointElemType)
        val medianIn = inPoint(depth)

        val medianDist = RegNext((medianIn -& median) * (medianIn -& median))

        val isLeaf = outTreeRom(TreeRomCol.IsLeaf.id).asTypeOf(Bool())

        val hasLeftChild = rootTreeIdx < (numNodes.U / 2.U)

        val hasRightChild: Bool = rootTreeIdx < ((numNodes.U - 1.U) / 2.U)

        val leftChildIdx: UInt = (rootTreeIdx * 2.U) + 1.U

        val rightChildIdx: UInt = (rootTreeIdx * 2.U) + 2.U

        val inLeft: Bool = {
          medianIn < median
        }

//        printf(p"index = $rootTreeIdx; isLeaf = $isLeaf; median = $median; depth = $depth; medianIn = $medianIn... ")

        when(calcMedianDist) {
          calcMedianDist := false.B
          when(medianDist < closestPointDist) {
            val secondChildIdx = Mux(inLeft, rightChildIdx, leftChildIdx)
//            printf(p"Push second child ($secondChildIdx)\n")
            popAndPushStacks(secondChildIdx, false.B, nextDepth)
          }.otherwise {
//            printf("Don't push second child\n")
            popStacks()
          }
        }.elsewhen(isLeaf) {
//          retrievedTreeRom := true.B
//          printf(p"addPRom = $addrPRom; Checking point $iterPointsVisited of $pLen (${outPRom})... ")
          when(iterPointsVisited < pLen - 1.U) {
            iterPointsVisited := iterPointsVisited + 1.U
          }
          when(iterPointsVisited === pLen - 1.U) {
            popStacks()
            iterPointsVisited := 0.U
            closestPointDelay := 2.U
//            stoppedProcessingPoints := true.B
//            printf("That was the last point\n")
          }.otherwise {
//            printf("\n")
          }
        }.elsewhen(tryInspectSecondChild) {
//          printf("Try inspect second child\n")
          chisel3.assert(hasRightChild)
          calcMedianDist := true.B
        }.otherwise {
          chisel3.assert(hasLeftChild)
          when(hasRightChild) {
            val firstChildIdx = Mux(inLeft, leftChildIdx, rightChildIdx)
//            printf(p"Enqueue first child ($firstChildIdx)\n")
            childStack.top := true.B
            pushStacks(firstChildIdx, false.B, nextDepth)
          }.otherwise {
//            printf(p"Enqueue only child ($leftChildIdx)\n")
            popAndPushStacks(leftChildIdx, false.B, nextDepth)
          }
        }
//      }
    }
    .elsewhen(closestPointDelay === 0.U) {
      validResult := true.B
      busy := !io.out.ready

      // Prepare for next input point
      pushStacks(0.U, false.B, 0.U, assignTreeRom = false)
      closestPointAssigned := false.B
//      printf("Done, io.out.ready = %d; busy = %d\n", io.out.ready, busy)
    }
  }
}

object KdTreeModule {
  //  def fromKdTree[A <: IndexedSeq[C], C: Ordering](kdTree: KdTreeT[A, C], pointElemWidth: Int, pRomAddressWidth: Int): KdTreeModule = {
  //    new KdTreeModule(pointElemWidth, pRomAddressWidth, log2Up(kdTree.count + 1), kdTree.numDimensions, KdTreeT.parseStringSpec[C](kdTree.stringSpec))
  //  }
  def fromJson[A <: IndexedSeq[Int]](json: String, pRomFilename: String = "p_rom.mem", treeRomFilename: String = "tree_rom.mem"): KdTreeModule = {
    val (d, c, n, p_a, tree_rom_init, p_rom_init) = KdTreeT.parseJson[Int](json)
    new KdTreeModule(c, p_a, n, d, (tree_rom_init, p_rom_init), pRomFilename, treeRomFilename)
  }
  def fromKdTree[A <: IndexedSeq[Int]](kdTree: KdTree[A, Int], pointElemWidth: Int, pRomAddressWidth: Int, pRomFilename: String = "p_rom.mem", treeRomFilename: String = "tree_rom.mem"): KdTreeModule = {
    fromJson[A](kdTree.json(pointElemWidth, pRomAddressWidth), pRomFilename, treeRomFilename)
  }
}

//noinspection TypeAnnotation
class KdTreeModuleDriver(val pointElemWidth: Int, points: Seq[IndexedSeq[Int]]) extends Module {
  require(points.nonEmpty && points.forall(_.length == points.head.length))
  val nDim = points.head.length
  val io = IO(new Bundle {
    val start = Input(Bool())
    val out = Decoupled(Vec(nDim, SInt(pointElemWidth.W)))
  })
  val pMem = VecInit(points.map(p => VecInit(p.map { _.S(pointElemWidth.W) } )))

  val started = RegInit(false.B)
  private val (count, _) = Counter(started && io.out.ready, pMem.length)
//  printf(p"started == $started; io.out.ready == ${io.out.ready}; count == $count; driverOut == ${io.out.bits}\n")
  when(started) {
    io.out.valid := io.out.ready
    io.out.bits := pMem(count)
  }.otherwise {
    io.out.valid := false.B
    io.out.bits := VecInit(Seq.fill(nDim)(0.S(pointElemWidth.W)))
  }

  when(io.start) {
    started := true.B
  }
}

//noinspection TypeAnnotation
class KdTreeModuleWithDriver(driverCBN: => KdTreeModuleDriver, moduleCBN: => KdTreeModule) extends Module {
  val driver = driverCBN
  val module = moduleCBN
  require(driver.pointElemWidth == module.pointElemWidth)
  require(driver.nDim == module.pointNumDimensions)
  val io = IO(new Bundle {
    val start = Input(Bool())
//    val passthrough = Output(Vec(driver.nDim, SInt(driver.pointElemWidth.W)))
    val out = Decoupled(Vec(driver.nDim, SInt(driver.pointElemWidth.W)))
  })

  driver.io.start := io.start
  module.io.in <> driver.io.out
  io.out <> module.io.out
//  io.passthrough := module.io.inPoint
}
