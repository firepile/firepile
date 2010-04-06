package simplecl

import com.nativelibs4java.opencl._
import java.util.EnumSet

class SCLDevice(cld: CLDevice) {
  val _CLDevice: CLDevice = cld

  def createOutOfOrderQueue(sclc: SCLContext): SCLQueue = {
    new SCLQueue(cld.createOutOfOrderQueue(sclc._CLContext))
  }

  def createProfilingQueue(sclc: SCLContext): SCLQueue = {
    new SCLQueue(cld.createProfilingQueue(sclc._CLContext))
  }

  def createQueue(sclc: SCLContext, queueProperties: SCLDeviceUtil.QueueProperties*): SCLQueue = {
    new SCLQueue(cld.createQueue(sclc._CLContext, queueProperties:_*))
  }

  // def equals

  def addressBits: Int = cld.getAddressBits

  def driverVersion: String = cld.getDriverVersion

  def executionCapabilities: EnumSet[SCLDeviceUtil.ExecutionCapability] = cld.getExecutionCapabilities

  def extensions: List[String] = cld.getExtensions.toList

  def globalMemCachelineSize: Int = cld.getGlobalMemCachelineSize

  def globalMemCacheSize: Long = cld.getGlobalMemCacheSize

  // globalMemCacheType

  def globalMemSize: Long = cld.getGlobalMemSize

  // image2DMaxHeight / image2DMaxWidth / image3DMaxDepth / image3DMaxHeight / image3DMaxWidth

  def localMemSize: Long = cld.getLocalMemSize

  // localMemType: CLDevice.LocalMemType

  def maxClockFrequency: Int = cld.getMaxClockFrequency

  def maxComputeUnits: Int = cld.getMaxComputeUnits

  def maxConstantArgs: Int = cld.getMaxConstantArgs

  def maxConstantBufferSize: Long = cld.getMaxConstantBufferSize

  def maxMemAllocSize: Long = cld.getMaxMemAllocSize

  def maxParameterSize: Long = cld.getMaxParameterSize

  def maxReadImageArgs: Int = cld.getMaxReadImageArgs

  def maxSamplers: Int = cld.getMaxSamplers

  def maxWorkGroupSize: Long = cld.getMaxWorkGroupSize

  def maxWorkItemDimenstions: Int = cld.getMaxWorkItemDimensions

  def maxWorkItemSizes: List[Long] = cld.getMaxWorkItemSizes.toList 

  def maxWriteImageArgs: Int = cld.getMaxWriteImageArgs

  def memBaseAddrAlign: Int = cld.getMemBaseAddrAlign

  def minDataTypeAlign: Int = cld.getMinDataTypeAlign

  def name: String = cld.getName

  def platform: SCLPlatform = new SCLPlatform(cld.getPlatform)

  def preferredVectorWidthChar: Int = cld.getPreferredVectorWidthChar

  def preferredVectorWidthDouble: Int = cld.getPreferredVectorWidthDouble

  def preferredVectorWidthFloat: Int = cld.getPreferredVectorWidthFloat

  def preferredVectorWidthInt: Int = cld.getPreferredVectorWidthInt

  def preferredVectorWidthLong: Int = cld.getPreferredVectorWidthLong

  def preferredVectorWidthShort: Int = cld.getPreferredVectorWidthShort

  def profile: String = cld.getProfile

  def profilingTimerResolution: Long = cld.getProfilingTimerResolution

  def queueProperties: EnumSet[SCLDeviceUtil.QueueProperties] = cld.getQueueProperties

  def singleFPConfig: EnumSet[SCLDeviceUtil.SingleFPConfig] = cld.getSingleFPConfig

  def getType: EnumSet[SCLDeviceUtil.Type] = cld.getType

  def vendor: String = cld.getVendor

  def vendorId: Int = cld.getVendorId

  def version: String = cld.getVersion

  def errorCorrectionSupport: Boolean = cld.hasErrorCorrectionSupport

  // def hashCode: Int = cld.hashCode

  def hasImageSupport: Boolean = cld.hasImageSupport

  def isAvailable: Boolean = cld.isAvailable

  def isByteAddressableStoreSupported: Boolean = cld.isByteAddressableStoreSupported

  def isCompilerAvailable: Boolean = cld.isCompilerAvailable

  def isDoubleSupported: Boolean = cld.isDoubleSupported

  def isEndianLittle: Boolean = cld.isEndianLittle

  def isGlobalInt32BaseAtomicsSupported: Boolean = cld.isGlobalInt32BaseAtomicsSupported

  def isGlobalInt32ExtendedAtomicsSupported: Boolean = cld.isGlobalInt32ExtendedAtomicsSupported

  def isGLSharingSupported: Boolean = cld.isGLSharingSupported

  def isHalfSupported: Boolean = cld.isHalfSupported

  def isLocalInt32BaseAtomicsSupported: Boolean = cld.isLocalInt32BaseAtomicsSupported

  def isLocalInt32ExtendedAtomicsSupported: Boolean = cld.isLocalInt32ExtendedAtomicsSupported

  def release: Unit = cld.release

  // def toString: String = cld.toString

}

//class SCLDeviceType extends Enumeration {
  //	type SCLDeviceType = Value
  //	val CPU = Value(library.OpenCLLibrary.CL_DEVICE_TYPE_CPU)
  //	val GPU = Value(library.OpenCLLibrary.CL_DEVICE_TYPE_GPU)
  //	val Accelerator = Value(library.OpenCLLibrary.CL_DEVICE_TYPE_ACCELERATOR)
  //	val Default = Value(library.OpenCLLibrary.CL_DEVICE_TYPE_DEFAULT)
  //
  //}

  object SCLDeviceUtil {

    type Type = CLDevice.Type
    type QueueProperties = CLDevice.QueueProperties
    type ExecutionCapability = CLDevice.ExecutionCapability
    type SingleFPConfig = CLDevice.SingleFPConfig

  }
