package firepile.tests

object DCT8x8 {

  import firepile._
  import firepile.Device
  import firepile.Space
  import firepile.Arrays._
  import firepile.Kernel._
  import firepile.util.BufferBackedArray._
  import firepile.tree.Trees.Tree
  import com.nativelibs4java.opencl._
  import com.nativelibs4java.util._
  import java.nio.FloatBuffer
  import java.nio.ByteOrder
  import scala.collection.JavaConversions._
  import firepile.Marshaling._
  import scala.util.Random
 
  
  var NUM_ITEMS = 16384 // 1048576
  var stride = 2048
  var width = 0
  var height = 0
  val BLOCK_X = 32
  val BLOCK_Y = 16
  val BLOCK_SIZE = 8
 
  def main(args: Array[String]) = {

  if (args.length > 0) NUM_ITEMS= if (args.length > 0) (1 << args(0).toInt) else ( 1 << 11)

  width = NUM_ITEMS
  height = NUM_ITEMS

  run
  	
  }

  def iDivUp(dividend: Int, divisor: Int) = {
    dividend / divisor + (if (dividend % divisor != 0) 1 else 0)
  }
  

  def run = {
    val random = new Random(2009)
    val randInput = Array.fill(height * stride)(random.nextFloat)
    val result = DCT8x8(BBArray.fromArray(randInput))(firepile.gpu)


  }

def DCT8x8(src: BBArray[Float])
 (implicit dev: Device): Array[Float] = {

  val BLOCK_X = 32
  val BLOCK_Y = 16
  val BLOCK_SIZE = 8
  val imageW = width
  val imageH = height
  
  val space = dev.defaultPaddedPartition(src.length)
  val gs1 = iDivUp(imageW, BLOCK_X) * BLOCK_X
  val gs2 = iDivUp(imageH, BLOCK_Y) * (BLOCK_Y / BLOCK_SIZE)
  dev.setWorkSizes(Array(gs1, gs2),Array(BLOCK_X, BLOCK_Y / BLOCK_SIZE))
  val dst = BBArray.ofDim[Float](src.length)
  
  println("Block size = " + space.blocks)
  Kernel.output("dst")
 
  space.spawn { 
   
  space.groups.foreach {
    g => {
        val l_Transpose = Array.ofDim[Float](BLOCK_Y * (BLOCK_X+1))  
   
        g.items.foreach {
          item => { 
            val BLOCK_SIZE = 8
            val localX = item.id(0)
            val localY = BLOCK_SIZE * item.id(1)

            val modLocalX = localX & (BLOCK_SIZE - 1)
            val globalX = g.id(0) * BLOCK_X + localX
            val globalY = g.id(1) * BLOCK_Y + localY

            if (!((globalX - modLocalX + BLOCK_SIZE - 1 >= imageW) || (globalY + BLOCK_SIZE - 1 >= imageH))) {

            val D = new Array[Float](8)

            var i = 0
            while (i < BLOCK_SIZE) {
              l_Transpose((localY * BLOCK_Y) + localX + (i * (BLOCK_X+1))) = src((globalY * stride + globalY) + (i * stride))
              i += 1
            }

            i = 0
            while (i < BLOCK_SIZE) {
              D(i) = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + i)
              i += 1
            }

            DCT8(D)

            i = 0
            while (i < BLOCK_SIZE) {
              l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + i) = D(i)
              i += 1
            }

            i = 0
            while (i < BLOCK_SIZE) {
              D(i) = l_Transpose((localY * BLOCK_Y) + localX + (i * (BLOCK_X+1)))
              i += 1
            }

            DCT8(D)

            i = 0
            while (i < BLOCK_SIZE) {
              dst((globalY * stride + globalY) + (i * stride)) = D(i)
              i += 1
            }
            } 
          }
        }
      }
    }
     // odata escapes -- so need to pass in to the generated kernel 
     (dst,src,stride,imageH,imageW)
     
  }
   
    dst
  }

  def DCT8(D: Array[Float]) = {
    val C_a = 1.3870398453221474618216191915664f  //a = sqrt(2) * cos(1 * pi / 16)
    val C_b = 1.3065629648763765278566431734272f  //b = sqrt(2) * cos(2 * pi / 16)
    val C_c = 1.1758756024193587169744671046113f  //c = sqrt(2) * cos(3 * pi / 16)
    val C_d = 0.78569495838710218127789736765722f //d = sqrt(2) * cos(5 * pi / 16)
    val C_e = 0.54119610014619698439972320536639f //e = sqrt(2) * cos(6 * pi / 16)
    val C_f = 0.27589937928294301233595756366937f //f = sqrt(2) * cos(7 * pi / 16)
    val C_norm = 0.35355339059327376220042218105242f //1 / sqrt(8)

    val X07P = D(0) + D(7)
    val X16P = D(1) + D(6)
    val X25P = D(2) + D(5)
    val X34P = D(3) + D(4)

    val X07M = D(0) - D(7)
    val X61M = D(6) - D(1)
    val X25M = D(2) - D(5)
    val X43M = D(4) - D(3)

    val X07P34PP = X07P + X34P
    val X07P34PM = X07P - X34P
    val X16P25PP = X16P + X25P
    val X16P25PM = X16P - X25P

    D(0) = C_norm * (X07P34PP + X16P25PP)
    D(2) = C_norm * (C_b * X07P34PM + C_e * X16P25PM)
    D(4) = C_norm * (X07P34PP - X16P25PP)
    D(6) = C_norm * (C_e * X07P34PM - C_b * X16P25PM)

    D(1) = C_norm * (C_a * X07M - C_c * X61M + C_d * X25M - C_f * X43M)
    D(3) = C_norm * (C_c * X07M + C_f * X61M - C_a * X25M + C_d * X43M)
    D(5) = C_norm * (C_d * X07M + C_a * X61M + C_f * X25M - C_c * X43M)
    D(7) = C_norm * (C_f * X07M + C_d * X61M + C_c * X25M + C_a * X43M)
  }
  
}
