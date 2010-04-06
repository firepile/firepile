package simplecl

import com.nativelibs4java.opencl._
import java.nio.Buffer

class SCLBuffer[B <: Buffer](clb: CLBuffer[B]) extends SCLMem(clb) {

}
