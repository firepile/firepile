package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile._

object Wrappers {
  class ArrayKernelWrapper[A: FixedSizeMarshal](a1: Array[A]) {
    def mapKernel[B: FixedSizeMarshal](k: BBArrayMapKernel1[A,B]) = k(a2bb(a1))
    def reduceKernel(k: BBArrayReduceKernel1[A]) = k(a2bb(a1))
    def lazyzip[A2: FixedSizeMarshal](a2:BBArray[A2]) = new BBSpawner2[A,A2](a2bb(a1),a2)
  }

  class BBArrayKernelWrapper[A: FixedSizeMarshal](a1: BBArray[A]) {
    def mapKernel[B: FixedSizeMarshal](k: BBArrayMapKernel1[A,B]) = k(a1)
    def reduceKernel(k: BBArrayReduceKernel1[A]) = k(a1)
    def lazyzip[A2: FixedSizeMarshal](a2:BBArray[A2]) = new BBSpawner2[A,A2](a1,a2)
  }

  class BBSpawner2[A1: FixedSizeMarshal,A2: FixedSizeMarshal](a1:BBArray[A1],a2:BBArray[A2]) {
    def lazyzip[A3: FixedSizeMarshal](a3:BBArray[A3]) = new BBSpawner3(a1,a2,a3)
    def zipWithKernel[B: FixedSizeMarshal](k: BBArrayMapKernel2[A1,A2,B]) = k(a1,a2)

  }
  class BBSpawner3[A1: FixedSizeMarshal,A2: FixedSizeMarshal,A3: FixedSizeMarshal](a1:BBArray[A1],a2:BBArray[A2],a3:BBArray[A3]) {
    def zipWithKernel[B: FixedSizeMarshal](k: BBArrayMapKernel3[A1,A2,A3,B]) = k(a1,a2,a3)
  }
}
