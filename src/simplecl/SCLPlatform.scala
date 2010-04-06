package simplecl

import com.nativelibs4java.opencl._
import java.util.EnumSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class SCLPlatform(clp: CLPlatform) {
  def _CLPlatform: CLPlatform = clp

  def bestDevice: CLDevice = clp.getBestDevice
  def name: String = clp.getName
  def profile: String = clp.getProfile
  def vendor: String = clp.getVendor
  def version: String = clp.getVersion
  def hasCode: Int = clp.hashCode
  def isByteAddressableStoreSupported: Boolean = clp.isByteAddressableStoreSupported
  def isGLSharingSupported: Boolean = clp.isGLSharingSupported
  def isNVCompilerOptionsSupported: Boolean = clp.isNVCompilerOptionsSupported
  def isNVDeviceAttributeQuerySupported: Boolean = clp.isNVDeviceAttributeQuerySupported


  def createContext(contextProperties: java.util.Map[SCLPlatformUtil.ContextProperties,Number], devices: SCLDevice*): SCLContext = {
    val a = new ArrayBuffer[CLDevice]
    devices.foreach(d => a += d._CLDevice)
    new SCLContext(clp.createContext(contextProperties, (a.toArray):_*))
  }

  def listAllDevices(onlyAvailable: Boolean): List[SCLDevice] = {
    val clds = clp.listAllDevices(onlyAvailable)
    val sclds = new ListBuffer[SCLDevice]()

    clds.foreach((cld: CLDevice) => sclds += new SCLDevice(cld))

    sclds.toList
  }

  def listCPUDevices(onlyAvailable: Boolean): List[SCLDevice] = {
    val clds = clp.listCPUDevices(onlyAvailable)
    val sclds = new ListBuffer[SCLDevice]()

    clds.foreach((cld: CLDevice) => sclds += new SCLDevice(cld))

    sclds.toList
  }

  def listGPUDevices(onlyAvailable: Boolean): List[SCLDevice] = {
    val clds = clp.listGPUDevices(onlyAvailable)
    val sclds = new ListBuffer[SCLDevice]()

    clds.foreach((cld: CLDevice) => sclds += new SCLDevice(cld))

    sclds.toList
  }

  // Not yet sure what to do about EnumSet, maybe a BitSet?
  def listDevices(types: EnumSet[SCLDeviceUtil.Type], onlyAvailable: Boolean): List[SCLDevice] = {
    val clds = clp.listDevices(types, onlyAvailable)
    val sclds = new ListBuffer[SCLDevice]()

    clds.foreach((cld: CLDevice) => sclds += new SCLDevice(cld))

    sclds.toList
  }

  def release: Unit = clp.release
  override def toString: String = clp.toString
}

object SCLPlatformUtil {
  type ContextProperties = CLPlatform.ContextProperties
}

