package simplecl

import com.nativelibs4java.opencl._

class SCLProgram(clprg: CLProgram) {
  def _CLProgram: CLProgram = clprg

  def build: SCLProgram = new SCLProgram(_CLProgram.build)
  def createKernel(name: String, args: Object*): SCLKernel = {
    new SCLKernel(_CLProgram.createKernel(name, args:_*))
  }
}

