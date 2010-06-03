package firepile

import java.nio.ByteBuffer

import com.nativelibs4java.opencl.CLKernel
import firepile.Dist
import firepile.Effect

class BufKernel(code: CLKernel, val dist: Dist, val effect: Effect) {
  def apply(a: ByteBuffer*) = new InstantiatedBufKernel(code, dist, effect, a:_*)
}

