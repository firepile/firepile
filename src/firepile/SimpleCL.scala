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

  def listGPUPoweredPlatforms: List[SCLPlatform] = {
    val clps = JavaCL.listGPUPoweredPlatforms
    val sclps = new ListBuffer[SCLPlatform]

    clps.foreach((clp: CLPlatform) => sclps += new SCLPlatform(clp))

    sclps.toList

  }


  def getBestDevice: SCLDevice = new SCLDevice(JavaCL.getBestDevice)
  
  def createContext(contextProperties: java.util.Map[CLPlatform.ContextProperties,Number], devices: SCLDevice*): SCLContext = { 
    val sclds = new ListBuffer[CLDevice]

    devices.foreach((cld: SCLDevice) => sclds += cld._CLDevice)
    
    new SCLContext(JavaCL.createContext(contextProperties, sclds.toArray:_*))
  }

  def unloadCompiler = JavaCL.unloadCompiler
}
