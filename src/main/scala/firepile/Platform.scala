package firepile

import firepile._
import firepile.util.BufferBackedArray._

import com.nativelibs4java.opencl._
import java.util.EnumSet

  class Platform(private[firepile] val clp: CLPlatform) {
    def bestDevice = new Device(this, clp.getBestDevice)

    def name: String = clp.getName
    def profile: String = clp.getProfile
    def vendor: String = clp.getVendor
    def version: String = clp.getVersion

    def isByteAddressableStoreSupported: Boolean = clp.isByteAddressableStoreSupported
    def isGLSharingSupported: Boolean = clp.isGLSharingSupported
    def isNVCompilerOptionsSupported: Boolean = clp.isNVCompilerOptionsSupported
    def isNVDeviceAttributeQuerySupported: Boolean = clp.isNVDeviceAttributeQuerySupported

    private[firepile] def createContext(devices: CLDevice*): CLContext = {
      val map: java.util.Map[CLPlatform.ContextProperties,Number] = null
      clp.createContext(map, devices:_*)
    }

    private[firepile] def createContext(contextProperties: java.util.Map[CLPlatform.ContextProperties,Number], devices: CLDevice*): CLContext = {
      clp.createContext(contextProperties, devices:_*)
    }

    def listCPUs(onlyAvailable: Boolean = true): List[Device] = listDevices(onlyAvailable, Device.Type.CPU)
    def listGPUs(onlyAvailable: Boolean = true): List[Device] = listDevices(onlyAvailable, Device.Type.GPU)
    def listDevices(onlyAvailable: Boolean = true): List[Device] = listDevices(onlyAvailable, Device.Type.CPU, Device.Type.GPU, Device.Type.Accelerator, Device.Type.Default)

    def listDevices(onlyAvailable: Boolean, types: Device.Type.Value*): List[Device] = {
      val set = java.util.EnumSet.noneOf(classOf[CLDevice.Type])

      /*
      if (types.isEmpty) {
        set.add(CLDevice.Type.CPU)
        set.add(CLDevice.Type.GPU)
        set.add(CLDevice.Type.Accelerator)
        set.add(CLDevice.Type.Default)
      }
      else {
      */
        for (t <- types) {
          t match {
            case Device.Type.CPU => set.add(CLDevice.Type.CPU)
            case Device.Type.GPU => set.add(CLDevice.Type.GPU)
            case Device.Type.Accelerator => set.add(CLDevice.Type.Accelerator)
            case Device.Type.Default => set.add(CLDevice.Type.Default)
          }
        }
      // }

      val devs = clp.listDevices(set, onlyAvailable)
      devs.map { (d:CLDevice) => new Device(this, d) }.toList
    }

    def release: Unit = clp.release
    override def toString: String = clp.toString
  }

