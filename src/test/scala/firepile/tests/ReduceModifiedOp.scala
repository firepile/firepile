package firepile.tests

object ReduceModifiedOp {

  import firepile._
  import firepile.Device
  import firepile.util.BufferBackedArray._
  import scala.collection.JavaConversions._
  import firepile.Marshaling._
  import scala.util.Random


  var NUM_ITEMS = 16384

  def main(args: Array[String]) = {

    if (args.length > 0) NUM_ITEMS = if (args.length > 0) (1 << args(0).toInt) else (1 << 20)
    run

  }

  def run = {
    val random = new Random(0)
    val randInput = Array.fill(NUM_ITEMS)(random.nextFloat)
    val cpuSum = randInput.sum
    val gpuSum = reduceModified(BBArray.fromArray(randInput).directCopy)(firepile.gpu, _ + _)

    println("CPU sum = " + cpuSum + "   GPU sum = " + gpuSum)

  }

  // def reduceModified(idata: Array[Float], f: (Float,Float) => Float)
  def reduceModified(idata: BBArray[Float])
                    (implicit dev: Device, f: (Float, Float) => Float): Float = {

    println("Passed a function: " + whatIsTypeName(f))

    val space = dev.defaultPaddedPartition(idata.length)
    val odata = BBArray.ofDim[Float](space.blocks).directCopy
    val n = idata.length
    Kernel.output("odata")

    space.spawnF {

      space.groups.foreach {
        g => {

          val sdata = Array.ofDim[Float](g.items.size)

          g.items.foreach {
            item => {

              val i = g.id * (g.items.size * 2) + item.id

              if (i < n) sdata(item.id) = idata(i)
              else sdata(item.id) = 0f

              if (i + g.items.size < n)
                sdata(item.id) = f(idata(i + g.items.size), sdata(item.id))

              g.barrier

              var k = g.items.size / 2

              while (k > 0) {
                if (item.id < k)
                  sdata(item.id) = f(sdata(item.id + k), sdata(item.id))
                g.barrier
                k >>= 1
              }


              if (item.id == 0)
                odata(g.id) = sdata(0)
            }
          }
        }
      }
      // odata escapes -- so need to pass in to the generated kernel
      (odata, idata, n)

    }

    odata.reduceLeft(f)
  }

}
