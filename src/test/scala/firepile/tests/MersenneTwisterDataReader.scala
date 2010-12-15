package firepile.tests

import java.io._
import firepile.util.Unsigned._

object MersenneTwisterDataReader {

val MT_RNG_COUNT= 4096
val seed: UInt = 777.toUInt

def main(args: Array[String]) = {

readData("MersenneTwister.dat")

}

def readData(fileName: String ): (Array[UInt],Array[UInt],Array[UInt],Array[UInt])  ={
            val fileinputstream = new FileInputStream(fileName)
            val numberBytes = fileinputstream.available()
            var bytearray = new Array[Byte](numberBytes)
            println(" Number Bytes ::"+ numberBytes)
            fileinputstream.read(bytearray);
            val a = new Array[UInt](MT_RNG_COUNT)
            val b = new Array[UInt](MT_RNG_COUNT)
            val c = new Array[UInt](MT_RNG_COUNT)
            val d = new Array[UInt](MT_RNG_COUNT)
            var i=0
            var j=0
            while(i< numberBytes && j < MT_RNG_COUNT){
            a(j) = bytearray(i).toUInt
            b(j) = bytearray(i+1).toUInt
            c(j) = bytearray(i+2).toUInt
            d(j) = seed
            j+=1
            i+=4
            }
            
            fileinputstream.close
            (a,b,c,d)
   }

}