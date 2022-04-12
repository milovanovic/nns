package example

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation

class KdTreeModuleSpec extends KdTreeModuleTestSpec {

  "KdTreeModule" should "find the closest point, with the same distance as KdTree (sanity check)" in {
    val treePoints = Seq(Vector(1, 2), Vector(3, 4), Vector(5, 6), Vector(7, 8), Vector(9, 10), Vector(11, 12), Vector(13, 14), Vector(15, 16))
    testWithGoldenModel(3, treePoints, 100, 6, 3, randomSeed = Some(0))
  }

  it should "find the closest point, with the same distance as KdTree (half-reliable test)" in {
    val treePoints = Seq(Vector(11, 3), Vector(1, 3), Vector(14, 11), Vector(17, 8), Vector(15, 4), Vector(15, 15), Vector(15, 18), Vector(19, 7), Vector(2, 1), Vector(5, 4))
    testWithGoldenModel(5, treePoints, 1000, 6, 4, randomSeed = Some(0))
  }

  it should "not hurry to output the closest point before the independent closest point calculations have finished" in {
    val points = Seq(IndexedSeq(7), IndexedSeq(6), IndexedSeq(5), IndexedSeq(4), IndexedSeq(3), IndexedSeq(2), IndexedSeq(1))
    val tree = KdTree[IndexedSeq[Int], Int](1, points)
    val baseName = "delayTest_KdTreeModuleSpec"
    def module = KdTreeModule.fromKdTree(tree, 8, 8, pRomFilename = s"pRom_$baseName.mem", treeRomFilename = s"treeRom_$baseName.mem")

    test(module).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.io.in.initSource().setSourceClock(dut.clock)
      dut.io.out.initSink().setSinkClock(dut.clock)
      dut.io.out.ready.poke(true.B)

      dut.io.in.valid.poke(true.B)
      dut.io.in.bits(0).poke(1.S)
      while (dut.io.out.valid.peek().litValue == 0) {
        dut.clock.step(1)
      }

//      println(s"Out: ${dut.io.out.bits(0).peek().litValue}")
      assertResult(1) { dut.io.out.bits(0).peek().litValue }
    }
  }

  // JSON tests
  performJsonTests { (jsonFile, n, i, treeDepth, numLeaves, pointsPerLeaf) =>
    it should s"find the closest point (tree depth is $treeDepth, $numLeaves leaves, $pointsPerLeaf points per leaf)" in {
      testJson(jsonFile, useVerilator = i > (n/2))
    }
  }
}
