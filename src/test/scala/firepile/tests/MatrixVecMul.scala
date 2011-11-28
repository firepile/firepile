package firepile.tests

import firepile._
import firepile.Device
import firepile.util.BufferBackedArray._
import scala.collection.JavaConversions._
import firepile.Marshaling._
import scala.util.Random

object MatrixVecMul {


  val width = 1100
  var height = 100
  val localWorkSize = 256

  def main(args: Array[String]) = {

    height = if (args.length > 0) (args(0).toInt) else 100
    run
  }

  def run = {


    val random = new Random(0)
    //val idata1 = Array.fill( width * height) (random.nextFloat)
    //val idata2 = Array.fill( width ) (random.nextFloat)
    val idata1 = BBArray.tabulate[Float](width * height)(i => random.nextFloat).directCopy
    val idata2 = BBArray.tabulate[Float](width)(i => random.nextFloat).directCopy
    val odata = matrixVecMul(idata1, idata2, width, height)(firepile.gpu)

    println("output")
    Kernel.printTime
    //for ( i <- 0 until odata.length)
    // println(" " +odata(i))


  }

  def matrixVecMul(idata1: BBArray[Float], idata2: BBArray[Float], width: Int, height: Int)(implicit dev: Device): BBArray[Float] = {

    val space = dev.defaultPaddedPartition(idata1.length)
    val globalWorkSize = height - (height % localWorkSize) + localWorkSize
    dev.setWorkSizes(globalWorkSize, localWorkSize)
    Kernel.output("odata")

    val n = idata1.length
    val odata = new BBArray[Float](height).directCopy

    space.spawn {

      space.groups.foreach {
        g => {

          g.items.foreach {
            item => {


              val y = item.globalId
              if (y < height) {
                var dotProduct = 0f

                var x = 0
                while (x < width) {

                  dotProduct += idata1((y * width) + x) * idata2(x)
                  x += 1
                }
                odata(y) = dotProduct
              }
            }
          }
        }

      }
      (odata, idata1, idata2, width, height)
    }
    odata
  }
} 
