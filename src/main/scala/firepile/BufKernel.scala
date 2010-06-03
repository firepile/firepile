package firepile

import java.nio.ByteBuffer

import com.nativelibs4java.opencl.CLKernel

class BufKernel(dev: Device, code: CLKernel, val dist: Dist, val effect: Effect) {
  def apply(a: ByteBuffer*) = new InstantiatedBufKernel(dev, code, dist, effect, a:_*)
}

