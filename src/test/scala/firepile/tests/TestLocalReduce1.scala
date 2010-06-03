package firepile.tests

import firepile._
import firepile.Device
import firepile.Spaces._
import firepile.util.BufferBackedArray._

object TestLocalReduce1 {
  def reduceSum(input: BBArray[Float])(output: BlockIndexed1[Float])(id: Id1, sdata: LocalThreadIndexed1[Float]): Unit = {
    val config = id.config
    val n = input.length
    val i = id.block * (config.numThreadsPerBlock * 2) + id.localThread
    val tid = id.localThread

    sdata(tid) = if (i < n) input(i) else 0
    if (i + config.numThreadsPerBlock < n)
        sdata(tid) += input(i+config.numThreadsPerBlock)

    sdata.barrier

    var s = config.numThreadsPerBlock / 2
    while (s > 32) {
        sdata(tid) += sdata(tid+s)
        sdata.barrier
        s /= 2
    }

    sdata.barrier

    if (tid < 32) {
      config.numThreadsPerBlock match {
        case x if x >= 64 => sdata(tid) += sdata(tid+32)
        case x if x >= 32 => sdata(tid) += sdata(tid+16)
        case x if x >= 16 => sdata(tid) += sdata(tid+8)
        case x if x >=  8 => sdata(tid) += sdata(tid+4)
        case x if x >=  4 => sdata(tid) += sdata(tid+2)
        case x if x >=  2 => sdata(tid) += sdata(tid+1)
      }
    }

    if (tid == 0)
      output(id.block) = sdata(tid)
  }

  def main(args: Array[String]) = {
    val dataSize = if (args.length > 0) args(0).toInt else 1000

    implicit val gpu: Device = firepile.gpu

    val b = BBArray.tabulate(dataSize)(_.toFloat)

    println("cl bbarray sum");
    {
      val c: Array[Float] = time {
        val result = spawn(reduceSum _)(b)
        result.force
      }
      println("c = " + c.toList)
      val correct = b.sum
      println("correct sum = " + correct)
      assert(c.sum == correct)
    }
  }
}
