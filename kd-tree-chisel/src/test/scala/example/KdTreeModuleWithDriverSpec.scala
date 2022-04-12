package example

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import firrtl.AnnotationSeq

//noinspection TypeAnnotation
class KdTreeModuleWithDriverSpec extends KdTreeModuleTestSpec {

  performJsonTests { (jsonFile, n, i, treeDepth, numLeaves, pointsPerLeaf) =>
    val (kdTreeModuleFunc, (nDim, pointElemWidth, _, _), inputs, _) = extractJson(jsonFile, memoryFilePostfix = "withDriver")
    it should s"output the same resulting points as KdTreeModule (tree depth is $treeDepth, $numLeaves leaves, $pointsPerLeaf points per leaf)" in {
      test {
        new Module {
          val pointType = Vec(nDim, SInt(pointElemWidth.W))
          val io = IO(new Bundle {
            val in = Flipped(Decoupled(pointType))
            val start = Input(Bool())
            val outModule = Decoupled(pointType)
            val outModuleWithDriver = Decoupled(pointType)
          })
          val module = Module(kdTreeModuleFunc());
          val moduleWithDriver = Module(new KdTreeModuleWithDriver(
            Module(new KdTreeModuleDriver(pointElemWidth, inputs)),
            Module(kdTreeModuleFunc())
          ))
          module.io.in <> io.in
          io.outModule <> module.io.out
          moduleWithDriver.io.start := io.start
          io.outModuleWithDriver <> moduleWithDriver.io.out
        }
      }.withAnnotations(if (i > n / 2) Seq(VerilatorBackendAnnotation) else Seq()) { dut =>
        var moduleOutputs = List[IndexedSeq[BigInt]]()
        var moduleWithDriverOutputs = List[IndexedSeq[BigInt]]()

        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.outModule.initSink().setSinkClock(dut.clock)
        dut.io.outModuleWithDriver.initSink().setSinkClock(dut.clock)
        dut.clock.setTimeout(10000)
        dut.io.outModule.ready.poke(true.B)
        dut.io.outModuleWithDriver.ready.poke(true.B)
        dut.io.start.poke(true.B)

        inputs.head.indices.foreach { i => dut.io.in.bits(i).poke(inputs.head(i).S(pointElemWidth.W)) }
        dut.io.in.valid.poke(true.B)
        dut.clock.step(1)

        while (moduleOutputs.length < inputs.length || moduleWithDriverOutputs.length < inputs.length) {
          if (dut.io.outModule.valid.peek().litValue == 1) {
            moduleOutputs :+= dut.io.outModule.bits.map { _.peek().litValue }

            if (moduleWithDriverOutputs.length >= moduleOutputs.length) {
              val lastIndex = moduleOutputs.length - 1
              assertResult { moduleWithDriverOutputs(lastIndex) } { moduleOutputs(lastIndex) }
            }

            if (moduleOutputs.length < inputs.length) {
              dut.io.in.valid.poke(true.B)
              val nextPoint = inputs(moduleOutputs.length)
              nextPoint.indices.foreach { i => dut.io.in.bits(i).poke(nextPoint(i).S(pointElemWidth.W)) }
            }
          } else {
            dut.io.in.valid.poke(false.B)
          }

          if (dut.io.outModuleWithDriver.valid.peek().litValue == 1) {
            moduleWithDriverOutputs :+= dut.io.outModuleWithDriver.bits.map { _.peek().litValue }

            if (moduleOutputs.length >= moduleWithDriverOutputs.length) {
              val lastIndex = moduleWithDriverOutputs.length - 1
              assertResult { moduleOutputs(lastIndex) } { moduleWithDriverOutputs(lastIndex) }
            }
          }

          dut.clock.step(1)
        }

      }

    }
  }
}
