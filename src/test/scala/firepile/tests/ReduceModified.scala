package firepile.tests

object ReduceModified {

  import firepile._
  import firepile.Device
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
    val gpuSum = reduceModified(randInput, _ + _)(firepile.gpu)

    println("CPU sum = " + cpuSum + "   GPU sum = " + gpuSum)

  }

  def reduceModified(idata: Array[Float], f: (Float, Float) => Float)
                    (implicit dev: Device): Float = {

    val space = dev.defaultPaddedPartition(idata.length)

    val odata = Array.ofDim[Float](space.blocks)
    val n = idata.length

    space.spawn {

      space.groups.foreach {
        g => {

          val sdata = Array.ofDim[Float](g.items.size)

          g.items.foreach {
            item => {

              val j = g.id * (g.items.size * 2) + item.id
              sdata(item.id) = if (j < n) idata(j) else 0f

              if (j + g.items.size < n)
                sdata(item.id) = f(sdata(item.id), idata(j + g.items.size))

              g.barrier

              var k = g.items.size / 2

              while (k > 0) {
                if (item.id < k)
                  sdata(item.id) = f(sdata(item.id), sdata(item.id + k))
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
