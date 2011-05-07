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

  if (args.length > 0) NUM_ITEMS= if (args.length > 0) (1 << args(0).toInt) else ( 1 << 7)

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
    val result = DCT8x8(BBArray.fromArray(randInput).directCopy)(firepile.gpu)
    Kernel.printTime

  }

def DCT8x8(src: BBArray[Float])
 (implicit dev: Device): Array[Float] = {

  val BLOCK_X = 32
  val BLOCK_Y = 16
  val BLOCK_SIZE = 8
  val imageW = width
  val imageH = height
  var stride = 2048
  
  val space = dev.defaultPaddedPartition(src.length)
  val gs1 = iDivUp(imageW, BLOCK_X) * BLOCK_X
  val gs2 = iDivUp(imageH, BLOCK_Y) * (BLOCK_Y / BLOCK_SIZE)
  dev.setWorkSizes(Array(gs1, gs2),Array(BLOCK_X, BLOCK_Y / BLOCK_SIZE))
  dev.setLocalMemSize(BLOCK_Y * (BLOCK_X+1))
  val dst = BBArray.ofDim[Float](src.length).directCopy
  
  println("Block size = " + space.blocks)
  Kernel.output("dst")
 
  space.spawn { 
   
  space.groups.foreach {
    g => {
        val l_Transpose = BBArray.ofDim[Float](BLOCK_Y * (BLOCK_X+1))  
   
        g.items.foreach {
          item => { 
            val BLOCK_SIZE = 8
            val BLOCK_X = 32
            val BLOCK_Y = 16

            val localX = item.id(0)
            val localY = BLOCK_SIZE * item.id(1)

            val modLocalX = localX & (BLOCK_SIZE - 1)
            val globalX = g.id(0) * BLOCK_X + localX
            val globalY = g.id(1) * BLOCK_Y + localY

            if (!((globalX - modLocalX + BLOCK_SIZE - 1 >= imageW) || (globalY + BLOCK_SIZE - 1 >= imageH))) {

            var D0 = 0f
            var D1 = 0f
            var D2 = 0f
            var D3 = 0f
            var D4 = 0f
            var D5 = 0f
            var D6 = 0f
            var D7 = 0f
            val C_a = 1.3870398453221474618216191915664f  //a = sqrt(2) * cos(1 * pi / 16)
            val C_b = 1.3065629648763765278566431734272f  //b = sqrt(2) * cos(2 * pi / 16)
            val C_c = 1.1758756024193587169744671046113f  //c = sqrt(2) * cos(3 * pi / 16)
            val C_d = 0.78569495838710218127789736765722f //d = sqrt(2) * cos(5 * pi / 16)
            val C_e = 0.54119610014619698439972320536639f //e = sqrt(2) * cos(6 * pi / 16)
            val C_f = 0.27589937928294301233595756366937f //f = sqrt(2) * cos(7 * pi / 16)
            val C_norm = 0.35355339059327376220042218105242f //1 / sqrt(8)


            var i = 0
            while (i < BLOCK_SIZE) {
              l_Transpose((localY * BLOCK_Y) + localX + (i * (BLOCK_X+1))) = src((globalY * stride + globalY) + (i * stride))
              i += 1
            }

            D0 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 0)
            D1 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 1)
            D2 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 2)
            D3 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 3)
            D4 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 4)
            D5 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 5)
            D6 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 6)
            D7 = l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 7)

            // DCT8(D0,D1,D3,D3,D4,D5,D6,D7)
            var X07P = D0 + D7
            var X16P = D1 + D6
            var X25P = D2 + D5
            var X34P = D3 + D4

            var X07M = D0 - D7
            var X61M = D6 - D1
            var X25M = D2 - D5
            var X43M = D4 - D3

            var X07P34PP = X07P + X34P
            var X07P34PM = X07P - X34P
            var X16P25PP = X16P + X25P
            var X16P25PM = X16P - X25P

            D0 = C_norm * (X07P34PP + X16P25PP)
            D2 = C_norm * (C_b * X07P34PM + C_e * X16P25PM)
            D4 = C_norm * (X07P34PP - X16P25PP)
            D6 = C_norm * (C_e * X07P34PM - C_b * X16P25PM)

            D1 = C_norm * (C_a * X07M - C_c * X61M + C_d * X25M - C_f * X43M)
            D3 = C_norm * (C_c * X07M + C_f * X61M - C_a * X25M + C_d * X43M)
            D5 = C_norm * (C_d * X07M + C_a * X61M + C_f * X25M - C_c * X43M)
            D7 = C_norm * (C_f * X07M + C_d * X61M + C_c * X25M + C_a * X43M)


            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 0) = D0
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 1) = D1
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 2) = D2
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 3) = D3
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 4) = D4
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 5) = D5
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 6) = D6
            l_Transpose(((localY+modLocalX) * BLOCK_Y) + (localX - modLocalX) + 7) = D7

            D0 = l_Transpose((localY * BLOCK_Y) + localX + (0 * (BLOCK_X+1)))
            D1 = l_Transpose((localY * BLOCK_Y) + localX + (1 * (BLOCK_X+1)))
            D2 = l_Transpose((localY * BLOCK_Y) + localX + (2 * (BLOCK_X+1)))
            D3 = l_Transpose((localY * BLOCK_Y) + localX + (3 * (BLOCK_X+1)))
            D4 = l_Transpose((localY * BLOCK_Y) + localX + (4 * (BLOCK_X+1)))
            D5 = l_Transpose((localY * BLOCK_Y) + localX + (5 * (BLOCK_X+1)))
            D6 = l_Transpose((localY * BLOCK_Y) + localX + (6 * (BLOCK_X+1)))
            D7 = l_Transpose((localY * BLOCK_Y) + localX + (7 * (BLOCK_X+1)))

            // DCT8(D0,D1,D2,D3,D4,D5,D6,D7)
            X07P = D0 + D7
            X16P = D1 + D6
            X25P = D2 + D5
            X34P = D3 + D4

            X07M = D0 - D7
            X61M = D6 - D1
            X25M = D2 - D5
            X43M = D4 - D3

            X07P34PP = X07P + X34P
            X07P34PM = X07P - X34P
            X16P25PP = X16P + X25P
            X16P25PM = X16P - X25P

            D0 = C_norm * (X07P34PP + X16P25PP)
            D2 = C_norm * (C_b * X07P34PM + C_e * X16P25PM)
            D4 = C_norm * (X07P34PP - X16P25PP)
            D6 = C_norm * (C_e * X07P34PM - C_b * X16P25PM)

            D1 = C_norm * (C_a * X07M - C_c * X61M + C_d * X25M - C_f * X43M)
            D3 = C_norm * (C_c * X07M + C_f * X61M - C_a * X25M + C_d * X43M)
            D5 = C_norm * (C_d * X07M + C_a * X61M + C_f * X25M - C_c * X43M)
            D7 = C_norm * (C_f * X07M + C_d * X61M + C_c * X25M + C_a * X43M)

            dst((globalY * stride + globalY) + (0 * stride)) = D0
            dst((globalY * stride + globalY) + (1 * stride)) = D1
            dst((globalY * stride + globalY) + (2 * stride)) = D2
            dst((globalY * stride + globalY) + (3 * stride)) = D3
            dst((globalY * stride + globalY) + (4 * stride)) = D4
            dst((globalY * stride + globalY) + (5 * stride)) = D5
            dst((globalY * stride + globalY) + (6 * stride)) = D6
            dst((globalY * stride + globalY) + (7 * stride)) = D7
            } 
          }
        }
      }
    }
     (dst,src,stride,imageH,imageW)
     
  }
   
    dst
  }

  /*
  def DCT8(D0: Float, D1: Float, D2: Float, D3: Float, D4: Float, D5: Float, D6: Float, D7: Float) = {
    val C_a = 1.3870398453221474618216191915664f  //a = sqrt(2) * cos(1 * pi / 16)
    val C_b = 1.3065629648763765278566431734272f  //b = sqrt(2) * cos(2 * pi / 16)
    val C_c = 1.1758756024193587169744671046113f  //c = sqrt(2) * cos(3 * pi / 16)
    val C_d = 0.78569495838710218127789736765722f //d = sqrt(2) * cos(5 * pi / 16)
    val C_e = 0.54119610014619698439972320536639f //e = sqrt(2) * cos(6 * pi / 16)
    val C_f = 0.27589937928294301233595756366937f //f = sqrt(2) * cos(7 * pi / 16)
    val C_norm = 0.35355339059327376220042218105242f //1 / sqrt(8)

    val X07P = D0 + D7
    val X16P = D1 + D6
    val X25P = D2 + D5
    val X34P = D3 + D4

    val X07M = D0 - D7
    val X61M = D6 - D1
    val X25M = D2 - D5
    val X43M = D4 - D3

    val X07P34PP = X07P + X34P
    val X07P34PM = X07P - X34P
    val X16P25PP = X16P + X25P
    val X16P25PM = X16P - X25P

    D0 = C_norm * (X07P34PP + X16P25PP)
    D2 = C_norm * (C_b * X07P34PM + C_e * X16P25PM)
    D4 = C_norm * (X07P34PP - X16P25PP)
    D6 = C_norm * (C_e * X07P34PM - C_b * X16P25PM)

    D1 = C_norm * (C_a * X07M - C_c * X61M + C_d * X25M - C_f * X43M)
    D3 = C_norm * (C_c * X07M + C_f * X61M - C_a * X25M + C_d * X43M)
    D5 = C_norm * (C_d * X07M + C_a * X61M + C_f * X25M - C_c * X43M)
    D7 = C_norm * (C_f * X07M + C_d * X61M + C_c * X25M + C_a * X43M)
  }
  */
  
}
