package simplecl

import com.nativelibs4java.opencl._
import scala.collection.mutable.ListBuffer

object SimpleCL extends JavaCL {
  def listPlatforms: List[SCLPlatform] = {
    val clps = JavaCL.listPlatforms
    val sclps = new ListBuffer[SCLPlatform]

    clps.foreach((clp: CLPlatform) => sclps += new SCLPlatform(clp))

    sclps.toList

  }
  def listGPUPoweredPlatforms: List[CLPlatform] = JavaCL.listGPUPoweredPlatforms.toList


  def getBestDevice: CLDevice = JavaCL.getBestDevice
  def createContext(contextProperties: java.util.Map[CLPlatform.ContextProperties,Number], devices: CLDevice*): CLContext = JavaCL.createContext(contextProperties, devices:_*)
    def unloadCompiler = JavaCL.unloadCompiler
}
