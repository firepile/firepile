package firepile

import firepile._
import firepile.util.BufferBackedArray._

import java.nio.ByteBuffer

import scala.reflect.Manifest
import scala.collection.mutable.ArraySeq

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLDevice
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLKernel.LocalSize

import com.nativelibs4java.opencl._
import java.util.EnumSet

import Mems._

import scala.collection.JavaConversions._

  class DeviceLike(platform: Platform, cld: CLDevice) {
    private[firepile] lazy val context = platform.createContext(cld)

    def createQueue(properties: Device.QueueProperties.Value*) = {
      val props: Seq[CLDevice.QueueProperties] = properties.map {
        case Device.QueueProperties.OutOfOrder => CLDevice.QueueProperties.OutOfOrderExecModeEnable
        case Device.QueueProperties.Profiling => CLDevice.QueueProperties.ProfilingEnable
      }
      cld.createQueue(context, props:_*)
    }

    def addressBits: Int = cld.getAddressBits

    def driverVersion: String = cld.getDriverVersion

    def executionCapabilities = {
      val set: java.util.EnumSet[CLDevice.ExecutionCapability] = cld.getExecutionCapabilities
      val s2: Set[CLDevice.ExecutionCapability] = set.toSet
      s2.toList.map {
        case CLDevice.ExecutionCapability.Kernel => Device.ExecutionCapability.Kernel
        case CLDevice.ExecutionCapability.NativeKernel => Device.ExecutionCapability.NativeKernel
      }
    }

    def extensions: List[String] = cld.getExtensions.toList

    def globalMemCachelineSize: Int = cld.getGlobalMemCachelineSize
    def globalMemCacheSize: Long = cld.getGlobalMemCacheSize

    def globalMemCacheType = cld.getGlobalMemCacheType match {
      case CLDevice.GlobalMemCacheType.None => Device.MemCacheType.None
      case CLDevice.GlobalMemCacheType.ReadOnlyCache => Device.MemCacheType.ReadOnly
      case CLDevice.GlobalMemCacheType.ReadWriteCache => Device.MemCacheType.ReadWrite
    }

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
    def maxWorkItemSize1: Long = maxWorkItemSizes(0)
    def maxWorkItemSize2: (Long,Long) = (maxWorkItemSizes(0), maxWorkItemSizes(1))
    def maxWorkItemSize3: (Long,Long,Long) = (maxWorkItemSizes(0), maxWorkItemSizes(1), maxWorkItemSizes(2))

    def maxWriteImageArgs: Int = cld.getMaxWriteImageArgs

    def memBaseAddrAlign: Int = cld.getMemBaseAddrAlign

    def minDataTypeAlign: Int = cld.getMinDataTypeAlign

    def name: String = cld.getName

    def preferredVectorWidthChar: Int = cld.getPreferredVectorWidthChar

    def preferredVectorWidthDouble: Int = cld.getPreferredVectorWidthDouble

    def preferredVectorWidthFloat: Int = cld.getPreferredVectorWidthFloat

    def preferredVectorWidthInt: Int = cld.getPreferredVectorWidthInt

    def preferredVectorWidthLong: Int = cld.getPreferredVectorWidthLong

    def preferredVectorWidthShort: Int = cld.getPreferredVectorWidthShort

    def profile: String = cld.getProfile

    def profilingTimerResolution: Long = cld.getProfilingTimerResolution

    def queueProperties: EnumSet[CLDevice.QueueProperties] = cld.getQueueProperties

    def singleFPConfig: EnumSet[CLDevice.SingleFPConfig] = cld.getSingleFPConfig

    def getType: EnumSet[CLDevice.Type] = cld.getType

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
