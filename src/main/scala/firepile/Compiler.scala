package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.Spaces._
import firepile.tree.Trees._
import firepile.Implicits._

import soot.{Type => SootType}

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLKernel
import com.nativelibs4java.opencl.CLByteBuffer

import compiler.JVM2CL.compileRoot
import compiler.JVM2CL.compileMethod
import compiler.JVM2CL.mangleName
import compiler.JVM2CL.methodName

import java.util.ArrayList
import java.nio.ByteBuffer

// TODO: remove most of this.

object Compiler {
  var numIterations = 16

  def typeSig(t: java.lang.Class[_]): String = t match {
    case t if t == java.lang.Boolean.TYPE => "Z"
    case t if t == java.lang.Byte.TYPE => "B"
    case t if t == java.lang.Short.TYPE => "S"
    case t if t == java.lang.Character.TYPE => "C"
    case t if t == java.lang.Integer.TYPE => "I"
    case t if t == java.lang.Long.TYPE => "L"
    case t if t == java.lang.Float.TYPE => "F"
    case t if t == java.lang.Double.TYPE => "D"
    case t if t == java.lang.Void.TYPE => "V"
    case t if t.isArray => "[" + typeSig(t.getComponentType)
    case t => "L" + t.getName.replace('.', '/') + ";"
  }

  def signature(m: java.lang.reflect.Method) =
    m.getName + "(" + m.getParameterTypes.toList.map(t => typeSig(t)).mkString("") + ")" + typeSig(m.getReturnType)

  def findAllMethods(src: AnyRef, arity: Int): Option[(String, List[Tree])] = {

    val gMethod = findGlobalMethod(src.getClass.getName, arity)

    gMethod match {

      case Some(x: java.lang.reflect.Method) => {
        Some((methodName(x), compileRoot(src.getClass.getName, Compiler.signature(x)).reverse))
/*
        compileMethod(src.getClass.getName, Compiler.signature(x))
        val lMethod = findLocalMethod(src.getClass.getName)

        lMethod match {
          case Some((x: java.lang.reflect.Method, cname2: String)) => {
            compileMethod(cname2, Compiler.signature(x))
            val kernelMethod = findKernelMethod(cname2)
            kernelMethod match {
              case Some((x: java.lang.reflect.Method, cname3: String)) => {
                return Some((methodName(x), compileRoot(cname3, Compiler.signature(x)).reverse))
              }
              case None => { println(" Not able to find the Kernel Code !!!"); return None }
            }
          }
          case None => { println(" Not able to find the method with Local variables !!!"); return None }
        }
*/
      }
      case None => { println(" Not able to find the method with Global variables !!!"); return None }
    }
    //treeList.add(compileRoot(k3.getName, Compiler.signature(m)).reverse)

  }

  def findGlobalMethod(cname1: String, arity: Int): Option[java.lang.reflect.Method] = {
    println("findGlobalMethod start")
    val k1 = Class.forName(cname1)
    for (m <- k1.getDeclaredMethods) {
      //println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
      // println(" return type name::" + m.getReturnType.getName)
      if (m.getReturnType.getName.startsWith("scala.Tuple" + arity)) {
        return Some(m)
      }
    }
    //println("findGlobalMethod end")
    None
  }

  def findLocalMethod(c1: String): Option[(java.lang.reflect.Method, String)] = {
    println("findLocalMethod start")

    var i = 1
    while (true) {
      try {
        val cname = c1 + "$$anonfun$apply$" + i.toString
        val k2 = Class.forName(cname)
        for (m <- k2.getDeclaredMethods) {
          //println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
          val pars = m.getParameterTypes
          if (pars.length > 0)
            if (pars(0).getName.startsWith("firepile.Group"))
              return Some((m, cname))
        }
      } catch {
        case e: ClassNotFoundException => { if (i >= 100) return (None) }
        case e: SecurityException => { if (i >= 100) return (None) }
        //case  _ => {println(" General Exception"); if(i>=100) return(None) } 

      }
      i += 1
    }

    //println("findLocalMethod end")
    None
  }

  def findKernelMethod(c2: String): Option[(java.lang.reflect.Method, String)] = {
    println("findKernelMethod start")
    //println(" Generating Kernel Code ::")
    var i = 1
    while (true) {
      try {

        val cname = c2 + "$$anonfun$apply$" + i.toString
        val k3 = Class.forName(cname)
        for (m <- k3.getDeclaredMethods) {
          //println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
          val pars = m.getParameterTypes
          if (pars.length > 0)
            if (pars(0).getName.startsWith("firepile.Item"))
              return Some((m, cname))
        }

      } catch {
        case e: ClassNotFoundException => { if (i >= 100) return (None) }
        case e: SecurityException => { if (i >= 100) return (None) }
        //case  _ => {if(i>=100) return(None)} 

      }
      i += 1
    }

    //println("findKernelMethod end")
    None
  }

  def compileNew[A1, A2, A3](tuple: Tuple3[A1, A2, A3], kernName: String, tree: String)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], dev: Device) = {

    println("CL code for kernel name " + kernName +" :\n" + tree)

    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)


    val implicitMarshal = new ArrayList[(Marshal[_], ByteBuffer, Int, Int, Int)]()
    implicitMarshal.add((implicitly[Marshal[A1]], implicitly[Marshal[A1]].toBuffer(tuple._1).head, implicitly[Marshal[A1]].sizes(tuple._1).head, implicitly[Marshal[A1]].sizes(1).head, implicitly[Marshal[A1]].sizes(tuple._1).head / implicitly[Marshal[A1]].sizes(1).head))
    implicitMarshal.add((implicitly[Marshal[A2]], implicitly[Marshal[A2]].toBuffer(tuple._2).head, implicitly[Marshal[A2]].sizes(tuple._2).head, implicitly[Marshal[A2]].sizes(1).head, implicitly[Marshal[A2]].sizes(tuple._2).head / implicitly[Marshal[A2]].sizes(1).head))
    implicitMarshal.add((implicitly[Marshal[A3]], implicitly[Marshal[A3]].toBuffer(tuple._3).head, implicitly[Marshal[A3]].sizes(tuple._3).head, implicitly[Marshal[A3]].sizes(1).head, implicitly[Marshal[A3]].sizes(tuple._3).head / implicitly[Marshal[A3]].sizes(1).head))
    val outputBuffers = new ArrayList[(CLByteBuffer, Int, Marshal[_], Int, Int)]()
    var maxInputItems: Int = 0
    var maxInputSize: Int = 0
    var maxOutputItems: Int = 0
    var maxOutputSize: Int = 0
    var numArrays: Int = 0

    for (i <- 0 until Kernel.globalArgs.size) {

      var output = false

      Kernel.globalArgs.get(i) match {

        case (name: String, typ: SootType, index: Int) => {
          for (j <- 0 until Kernel.outputArgs.size)
            if (name.startsWith(Kernel.outputArgs.get(j)))
              output = true
         
         // println(" variable name::"+name+"  index::"+index +"  type::"+ typ + "  output::"+output+"  loop index::"+ i)
          
          if (output) {
            val clBuf = dev.context.createByteBuffer(CLMem.Usage.Output, implicitMarshal.get(i)._3)
            outputBuffers.add(clBuf, implicitMarshal.get(i)._3, implicitMarshal.get(i)._1, index, implicitMarshal.get(i)._5)
            val nItems = implicitMarshal.get(i)._5
		  
            if (nItems > maxOutputItems) {
              maxOutputItems = nItems
              maxOutputSize = implicitMarshal.get(i)._4
            }
              
            kernBin.setArg(i+numArrays, clBuf)
          } else {
            val nItems = implicitMarshal.get(i)._5
            
            if (nItems > maxInputItems) {
              maxInputItems = nItems
              maxInputSize = implicitMarshal.get(i)._4
            }
 
            time({

              // firepile.compiler.JVM2CL.translateType(typ).asInstanceOf[ValueType].name match {
              firepile.compiler.JVM2CL.translateType(typ) match {
                case ValueType("int") => kernBin.setArg(i+numArrays, get(tuple,i).asInstanceOf[Int])
                case ValueType("float") => kernBin.setArg(i+numArrays, get(tuple,i).asInstanceOf[Float])
                case ValueType("long") => kernBin.setArg(i+numArrays, get(tuple,i).asInstanceOf[Long])
                case ValueType("double") => kernBin.setArg(i+numArrays, get(tuple,i).asInstanceOf[Double])
                case StructType(typName) => typName.replace("Array", "") match {
                  case "int" => {
                    kernBin.setArg(i+numArrays, get(tuple,i).asInstanceOf[Array[Int]])
                    numArrays += 1
                    kernBin.setArg(i+numArrays, implicitMarshal.get(i)._4)
                  }
                  case "float" => {
                    kernBin.setArg(i+numArrays, get(tuple,i).asInstanceOf[Array[Float]])
                    numArrays += 1
                    kernBin.setArg(i+numArrays, implicitMarshal.get(i)._4)
                  }
                  case "long" => kernBin.setArg(i, get(tuple,i).asInstanceOf[Array[Long]])
                  case "double" => kernBin.setArg(i, get(tuple,i).asInstanceOf[Array[Double]])
                  case _ => throw new RuntimeException("Trying to setArg with unsupported array type")
                }

                case x => {
                  kernBin.setArg(i, dev.context.createByteBuffer(CLMem.Usage.Input, implicitMarshal.get(i)._2, true))
                }
        
              }
              
            }, "Copy to GPU")
          }

        }
        case _ => {}

      }
    }

   //println(" output Buffer size::"+ outputBuffers.size)
   //println(" max Input size ::"+ maxInputSize + " max Input items ::" + maxInputItems)
   //println(" max Output size ::"+ maxOutputSize + " max Output items ::" + maxOutputItems)
   
   
    val threads = (if (maxInputItems < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(maxInputItems) / scala.math.log(2))) else dev.maxThreads).toInt

    time({
   
      if (dev.memConfig == null) {

        if (Kernel.localArgs.size > 0)
          kernBin.setLocalArg(3, threads * maxOutputSize)
        kernBin.enqueueNDRange(dev.queue, Array[Int](maxOutputItems * threads), Array[Int](threads))

      } else {
        //println(" Setting default arguments ")
        if (Kernel.localArgs.size > 0)
          kernBin.setLocalArg(3, dev.memConfig.localMemSize * maxOutputSize)
        kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
      }
      dev.queue.finish
    }, "GPU", numIterations)

    time({

      for (i <- 0 until outputBuffers.size) {

        outputBuffers.get(i) match {

          case (clBuf: CLByteBuffer, totalSize : Int, marshal: Marshal[_], index: Int, items: Int) => {
            //println(" total Size::"+ totalSize + " items ::"+ items)
            val bufOut = allocDirectBuffer(totalSize)
            clBuf.read(dev.queue, bufOut, true)
            bufOut.rewind
            Array.copy(marshal.fromBuffer(List(bufOut)), 0, get(tuple,index).asInstanceOf[AnyRef], 0, items)
          }

          case _ => {}

        }

      }

    }, "From GPU")
  }
  
  def get[A,B,C](t: Tuple3[A,B,C], i: Int) = {
  
  i match {
  
  case 0 => t._1 
  case 1 => t._2
  case 2 => t._3
  case _ => println(" Wrong Index !!!"); null
  
   }
  
  }
 

  def compileNew[A1, A2, A3, A4](a: A1, b: A2, c: A3, d: A4, kernName: String, tree: String)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], dev: Device) = {

    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head

    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)

    var bufA1: ByteBuffer = transA1.toBuffer(a).head
    var bufA2: ByteBuffer = null
    var bufA3: ByteBuffer = null
    var bufA4: ByteBuffer = null

    var bufA1CLBuf: CLByteBuffer = null
    var bufA2CLBuf: CLByteBuffer = null
    var bufA3CLBuf: CLByteBuffer = null
    var bufA4CLBuf: CLByteBuffer = null

    time({
      bufA2 = transA2.toBuffer(b).head
      bufA3 = transA3.toBuffer(c).head
      bufA4 = transA4.toBuffer(d).head

      bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
      bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
      bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA4, true)

    }, "Copy to GPU")

    val numItemsA1 = transA1.sizes(a).head / sizeA1
    val numItemsA2 = transA2.sizes(b).head / sizeA2
    val numItemsA3 = transA3.sizes(c).head / sizeA3
    val numItemsA4 = transA4.sizes(d).head / sizeA4

    val bufA1capacity = transA1.sizes(a).head

    bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA1capacity)

    // println("Output buffer capacity: " + bufA1capacity)

    val threads = (if (numItemsA2 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA2) / scala.math.log(2))) else dev.maxThreads).toInt

    // START TIMING CODE

    time({
      kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
      kernBin.setArg(1, bufA2CLBuf)
      kernBin.setArg(2, c.asInstanceOf[Int])
      kernBin.setArg(3, d.asInstanceOf[Int])

      //kernBin.setLocalArg(3, threads * sizeA1)

      if (dev.memConfig == null) {

        println(" Dev memConfig is null")
        // kernBin.setLocalArg(3, threads * sizeA1)
        kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA1 * threads), Array[Int](threads))
      } else {

        // println(" Setting default arguments ")
        // kernBin.setLocalArg(3, dev.memConfig.localMemSize * sizeA2)
        kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
      }

      //kernBin.enqueueNDRange(dev.queue, Array[Int](threads * numItemsA1 ), Array[Int](threads))
      dev.queue.finish
    }, "GPU", numIterations)

    val bufOut = allocDirectBuffer(bufA1capacity)

    time({
      bufA1CLBuf.read(dev.queue, bufOut, true)

      bufOut.rewind

      // [NN] maybe need to copy?  but, probably not
      Array.copy(transA1.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a.asInstanceOf[AnyRef], 0, numItemsA1)
    }, "From GPU")
    a
  }

  def compileNew[A1, A2, A3, A4, A5](a: A1, b: A2, c: A3, d: A4, e: A5, kernName: String, tree: String)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], ma5: Marshal[A5], dev: Device) = {

/*
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val transA5 = implicitly[Marshal[A5]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head
    val sizeA5 = transA5.sizes(1).head
*/
    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)
/*
    var bufA1: ByteBuffer = transA1.toBuffer(a).head
    var bufA2: ByteBuffer = null
    var bufA3: ByteBuffer = null
    var bufA4: ByteBuffer = null
    var bufA5: ByteBuffer = null
    var bufA1CLBuf: CLByteBuffer = null
    var bufA2CLBuf: CLByteBuffer = null
    var bufA3CLBuf: CLByteBuffer = null
    var bufA4CLBuf: CLByteBuffer = null
    var bufA5CLBuf: CLByteBuffer = null

    time({
      bufA2 = transA2.toBuffer(b).head
      bufA3 = transA3.toBuffer(c).head
      bufA4 = transA4.toBuffer(d).head
      bufA5 = transA5.toBuffer(e).head

      bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
      bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
      bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA4, true)
      bufA5CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA5, true)
    }, "Copy to GPU")

    val numItemsA1 = transA1.sizes(a).head / sizeA1
    val numItemsA2 = transA2.sizes(b).head / sizeA2
    val numItemsA3 = transA3.sizes(c).head / sizeA3
    val numItemsA4 = transA4.sizes(d).head / sizeA4
    val numItemsA5 = transA5.sizes(e).head / sizeA5
    val bufA1capacity = transA1.sizes(a).head

    bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA1capacity)

    // println("Output buffer capacity: " + bufA1capacity)

    val threads = (if (numItemsA2 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA2) / scala.math.log(2))) else dev.maxThreads).toInt

    // START TIMING CODE

    time({
      kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
      kernBin.setArg(1, bufA2CLBuf)
      kernBin.setArg(2, bufA3CLBuf)
      if (!Kernel.globalArgs.get(3)._2.equals("int"))
        kernBin.setArg(3, bufA4CLBuf)
      else
        kernBin.setArg(3, d.asInstanceOf[Int])
      if (!Kernel.globalArgs.get(3)._2.equals("int"))
        kernBin.setArg(3, bufA4CLBuf)
      else
        kernBin.setArg(4, e.asInstanceOf[Int])

      //kernBin.setLocalArg(3, threads * sizeA1)

      if (dev.memConfig == null) {

        println(" Dev memConfig is null")
        // kernBin.setLocalArg(3, threads * sizeA1)
        kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA1 * threads), Array[Int](threads))
      } else {

        // println(" Setting default arguments ")
        // kernBin.setLocalArg(3, dev.memConfig.localMemSize * sizeA2)
        kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
      }

      //kernBin.enqueueNDRange(dev.queue, Array[Int](threads * numItemsA1 ), Array[Int](threads))
      dev.queue.finish
    }, "GPU", numIterations)

    val bufOut = allocDirectBuffer(bufA1capacity)

    time({
      bufA1CLBuf.read(dev.queue, bufOut, true)

      bufOut.rewind

      // [NN] maybe need to copy?  but, probably not
      Array.copy(transA1.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a.asInstanceOf[AnyRef], 0, numItemsA1)
    }, "From GPU")
  */
    a
  }

  def findApplyMethod(src: AnyRef, arity: Int): java.lang.reflect.Method = {
    println(" Here::" + src)

    val cname = src.getClass.getName + "$$anonfun$apply$1"
    val k = Class.forName(cname)

    for (m <- k.getDeclaredMethods) {
      println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
      if (m.getParameterTypes.length == arity)
        if (m.getName.startsWith("apply$mc") && m.getName.endsWith("$sp"))
          return m
    }

    for (m <- k.getDeclaredMethods) {
      if (m.getParameterTypes.length == arity)
        if (m.getName.equals("apply"))
          return m
    }
    throw new RuntimeException("Could not find apply/" + arity + " method in " + k.getName)
  }

  var next = 0
  def freshName(base: String = "tmp") = {
    next += 1
    base + next
  }

  trait Kernel
  trait Kernel1[A] extends Function1[A, Unit] with Kernel
  trait Kernel2[A1, A2] extends Function2[A1, A2, Unit] with Kernel
  trait Kernel3[A1, A2, A3] extends Function3[A1, A2, A3, Unit] with Kernel
  trait Kernel4[A1, A2, A3, A4] extends Function4[A1, A2, A3, A4, Unit] with Kernel
  trait Kernel5[A1, A2, A3, A4, A5] extends Function5[A1, A2, A3, A4, A5, Unit] with Kernel
  trait Kernel6[A1, A2, A3, A4, A5, A6] extends Function6[A1, A2, A3, A4, A5, A6, Unit] with Kernel
  trait Kernel7[A1, A2, A3, A4, A5, A6, A7] extends Function7[A1, A2, A3, A4, A5, A6, A7, Unit] with Kernel

  def compile[A](f: A => Unit)(implicit ma: Marshal[A], dev: Device): Kernel1[A] = throw new RuntimeException("unimplemented")
  // e.g., reduce(input: Array[Int], output: Array[Int])
  // e.g., map(input: Array[Int], output: Array[Float])

  import scala.collection.mutable.HashMap
  val kernelCache = new HashMap[AnyRef, Kernel]
  // [NN] move to Device?
  def memoize[A1, A2](f: (A1, A2) => Unit)(k: => Kernel2[A1, A2]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel2[A1, A2]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3](f: (A1, A2, A3) => Unit)(k: => Kernel3[A1, A2, A3]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel3[A1, A2, A3]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Unit)(k: => Kernel4[A1, A2, A3, A4]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel4[A1, A2, A3, A4]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Unit)(k: => Kernel5[A1, A2, A3, A4, A5]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel5[A1, A2, A3, A4, A5]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4, A5, A6](f: (A1, A2, A3, A4, A5, A6) => Unit)(k: => Kernel6[A1, A2, A3, A4, A5, A6]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel6[A1, A2, A3, A4, A5, A6]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4, A5, A6, A7](f: (A1, A2, A3, A4, A5, A6, A7) => Unit)(k: => Kernel7[A1, A2, A3, A4, A5, A6, A7]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel7[A1, A2, A3, A4, A5, A6, A7]) =>
        println("found kernel in cache")
        k2
    }
  }

  def compile[A1, A2](f: (A1, A2) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], dev: Device): Kernel2[A1, A2] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 2) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

    new Kernel2[A1, A2] {
      def apply(a1: A1, a2: A2): Unit = {
        var bufA1: ByteBuffer = null
        var bufA1CLBuf: CLByteBuffer = null

        time({
          bufA1 = transA1.toBuffer(a1).head
          // val bufA2: ByteBuffer = transA2.toBuffer(a2).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val bufA2capacity = transA2.sizes(a2).head
        val bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA2capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        time({

          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(4, threads * sizeA1)
            kernBin.setArg(5, threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA2 * threads), Array[Int](threads))
          } else {
            kernBin.setLocalArg(4, dev.memConfig.localMemSize * sizeA1)
            kernBin.setArg(5, dev.memConfig.localMemSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA2capacity)

        time({
          bufA2CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA2.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a2.asInstanceOf[AnyRef], 0, numItemsA2)
        }, "Copy from GPU")
      }
    }
  }

  def compile[A1, A2, A3](f: (A1, A2, A3) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], dev: Device): Kernel3[A1, A2, A3] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 3) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)
    /*
    class Arg[A](val value: A, val marshal: Marshal[A]) {
        def toBuffers = marshal.toBuffer(value)
    }
    def applyKernel(args: Array[Arg[_]], output: Arg[_]): Unit = ...
*/

    new Kernel3[A1, A2, A3] {
      def apply(a1: A1, a2: A2, a3: A3): Unit = {
        var bufA1: ByteBuffer = null
        var bufA2: ByteBuffer = null
        // val bufA3: ByteBuffer = transA3.toBuffer(a3).head
        var bufA1CLBuf: CLByteBuffer = null
        var bufA2CLBuf: CLByteBuffer = null
        var bufA3CLBuf: CLByteBuffer = null

        time({
          bufA1 = transA1.toBuffer(a1).head
          bufA2 = transA2.toBuffer(a2).head
          // val bufA3: ByteBuffer = transA3.toBuffer(a3).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
          bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val numItemsA3 = transA3.sizes(a3).head / sizeA3
        val bufA3capacity = transA3.sizes(a3).head

        bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA3capacity)

        println("Output buffer capacity: " + bufA3capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        // START TIMING CODE

        time({
          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)
          kernBin.setArg(4, bufA3CLBuf)
          kernBin.setArg(5, numItemsA3)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(6, threads * sizeA1)
            kernBin.setArg(7, threads)
            println("Executing with global work size = " + (numItemsA3 * threads) + " and local work size = " + threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA3 * threads), Array[Int](threads))
          } else {
            // We don't really know if the local item types are the same as the global item types

            println("Executing with global work size = " + dev.memConfig.globalSize + " and local work size = " + dev.memConfig.localSize)
            kernBin.setLocalArg(6, dev.memConfig.localMemSize * sizeA1)
            kernBin.setArg(7, dev.memConfig.localMemSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA3capacity)

        time({
          bufA3CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA3.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a3.asInstanceOf[AnyRef], 0, numItemsA3)
        }, "From GPU")
      }
    }
  }

  def compile[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], dev: Device): Kernel4[A1, A2, A3, A4] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 4) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)
    /*
    class Arg[A](val value: A, val marshal: Marshal[A]) {
        def toBuffers = marshal.toBuffer(value)
    }
    def applyKernel(args: Array[Arg[_]], output: Arg[_]): Unit = ...
*/

    new Kernel4[A1, A2, A3, A4] {
      def apply(a1: A1, a2: A2, a3: A3, a4: A4): Unit = {
        var bufA1: ByteBuffer = null
        var bufA2: ByteBuffer = null
        var bufA3: ByteBuffer = null
        // val bufA4: ByteBuffer = transA4.toBuffer(a4).head
        var bufA1CLBuf: CLByteBuffer = null
        var bufA2CLBuf: CLByteBuffer = null
        var bufA3CLBuf: CLByteBuffer = null
        time({
          bufA1 = transA1.toBuffer(a1).head
          bufA2 = transA2.toBuffer(a2).head
          bufA3 = transA3.toBuffer(a3).head
          // val bufA4: ByteBuffer = transA4.toBuffer(a4).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
          bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
          bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val numItemsA3 = transA3.sizes(a3).head / sizeA3
        val numItemsA4 = transA4.sizes(a4).head / sizeA4
        val bufA4capacity = transA4.sizes(a4).head

        val bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA4capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        time({
          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)
          kernBin.setArg(4, bufA3CLBuf)
          kernBin.setArg(5, numItemsA3)
          kernBin.setArg(6, bufA4CLBuf)
          kernBin.setArg(7, numItemsA4)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(8, threads * sizeA1)
            kernBin.setArg(9, threads)
            println("Executing with global work size = " + (numItemsA2 * threads) + " and local work size = " + threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA2 * threads), Array[Int](threads))
          } else {
            // We don't really know if the local item types are the same as the global item types
            kernBin.setLocalArg(8, dev.memConfig.localSize * sizeA1)
            kernBin.setArg(9, dev.memConfig.localSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA4capacity)

        time({
          bufA4CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA4.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a4.asInstanceOf[AnyRef], 0, numItemsA4)
        }, "Copy from GPU")
      }
    }
  }

  def compile[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], ma5: Marshal[A5], dev: Device): Kernel5[A1, A2, A3, A4, A5] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val transA5 = implicitly[Marshal[A5]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head
    val sizeA5 = transA5.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 5) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

    new Kernel5[A1, A2, A3, A4, A5] {
      def apply(a1: A1, a2: A2, a3: A3, a4: A4, a5: A5): Unit = {
        var bufA1: ByteBuffer = null
        var bufA2: ByteBuffer = null
        var bufA3: ByteBuffer = null
        var bufA4: ByteBuffer = null
        // val bufA5: ByteBuffer = transA5.toBuffer(a5).head
        var bufA1CLBuf: CLByteBuffer = null
        var bufA2CLBuf: CLByteBuffer = null
        var bufA3CLBuf: CLByteBuffer = null
        var bufA4CLBuf: CLByteBuffer = null

        time({
          bufA1 = transA1.toBuffer(a1).head
          var bufA2 = transA2.toBuffer(a2).head
          var bufA3 = transA3.toBuffer(a3).head
          var bufA4 = transA4.toBuffer(a4).head
          // val bufA5: ByteBuffer = transA5.toBuffer(a5).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
          bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
          bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
          bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA4, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val numItemsA3 = transA3.sizes(a3).head / sizeA3
        val numItemsA4 = transA4.sizes(a4).head / sizeA4
        val numItemsA5 = transA5.sizes(a5).head / sizeA5
        val bufA5capacity = transA5.sizes(a5).head
        val bufA5CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA5capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        time({
          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)
          kernBin.setArg(4, bufA3CLBuf)
          kernBin.setArg(5, numItemsA3)
          kernBin.setArg(6, bufA4CLBuf)
          kernBin.setArg(7, numItemsA4)
          kernBin.setArg(8, bufA5CLBuf)
          kernBin.setArg(9, numItemsA5)

          println("Executing with global work size = " + dev.memConfig.globalSize + " and local work size = " + dev.memConfig.localSize)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(10, threads * sizeA5)
            kernBin.setArg(11, threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA5 * threads), Array[Int](threads))
          } else {
            // We don't really know if the local item types are the same as the global item types
            kernBin.setLocalArg(10, dev.memConfig.localMemSize * sizeA5)
            kernBin.setArg(11, dev.memConfig.localMemSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA5capacity)

        time({
          bufA5CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA5.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a5.asInstanceOf[AnyRef], 0, numItemsA5)
        }, "Copy from GPU")
      }
    }
  }

  def compile[A1, A2, A3, A4, A5, A6](f: (A1, A2, A3, A4, A5, A6) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], ma5: Marshal[A5], ma6: Marshal[A6], dev: Device): Kernel6[A1, A2, A3, A4, A5, A6] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val transA5 = implicitly[Marshal[A5]]
    val transA6 = implicitly[Marshal[A6]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head
    val sizeA5 = transA5.sizes(1).head
    val sizeA6 = transA6.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 6) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

    new Kernel6[A1, A2, A3, A4, A5, A6] {
      def apply(a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6): Unit = {
        var bufA1: ByteBuffer = null
        var bufA2: ByteBuffer = null
        var bufA3: ByteBuffer = null
        var bufA4: ByteBuffer = null
        var bufA5: ByteBuffer = null
        // val bufA6: ByteBuffer = transA6.toBuffer(a6).head

        var bufA1CLBuf: CLByteBuffer = null
        var bufA2CLBuf: CLByteBuffer = null
        var bufA3CLBuf: CLByteBuffer = null
        var bufA4CLBuf: CLByteBuffer = null
        var bufA5CLBuf: CLByteBuffer = null

        time({
          bufA1 = transA1.toBuffer(a1).head
          bufA2 = transA2.toBuffer(a2).head
          bufA3 = transA3.toBuffer(a3).head
          bufA4 = transA4.toBuffer(a4).head
          bufA5 = transA5.toBuffer(a5).head
          // val bufA6: ByteBuffer = transA6.toBuffer(a6).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
          bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
          bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
          bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA4, true)
          bufA5CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA5, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val numItemsA3 = transA3.sizes(a3).head / sizeA3
        val numItemsA4 = transA4.sizes(a4).head / sizeA4
        val numItemsA5 = transA5.sizes(a5).head / sizeA5
        val numItemsA6 = transA6.sizes(a6).head / sizeA6
        val bufA6capacity = transA6.sizes(a6).head
        val bufA6CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA6capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        time({
          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)
          kernBin.setArg(4, bufA3CLBuf)
          kernBin.setArg(5, numItemsA3)
          kernBin.setArg(6, bufA4CLBuf)
          kernBin.setArg(7, numItemsA4)
          kernBin.setArg(8, bufA5CLBuf)
          kernBin.setArg(9, numItemsA5)
          kernBin.setArg(10, bufA6CLBuf)
          kernBin.setArg(11, numItemsA6)

          println("Executing with global work size = " + dev.memConfig.globalSize + " and local work size = " + dev.memConfig.localSize)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(12, threads * sizeA1)
            kernBin.setArg(13, threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA2 * threads), Array[Int](threads))
          } else {
            // We don't really know if the local item types are the same as the global item types
            kernBin.setLocalArg(12, dev.memConfig.localMemSize * sizeA1)
            kernBin.setArg(13, dev.memConfig.localMemSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA6capacity)

        time({
          bufA6CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA6.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a6.asInstanceOf[AnyRef], 0, numItemsA6)
        }, "Copy from GPU")
      }
    }
  }

  def compile[A1, A2, A3, A4, A5, A6, A7](f: (A1, A2, A3, A4, A5, A6, A7) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], ma5: Marshal[A5], ma6: Marshal[A6], ma7: Marshal[A7], dev: Device): Kernel7[A1, A2, A3, A4, A5, A6, A7] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val transA5 = implicitly[Marshal[A5]]
    val transA6 = implicitly[Marshal[A6]]
    val transA7 = implicitly[Marshal[A7]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head
    val sizeA5 = transA5.sizes(1).head
    val sizeA6 = transA6.sizes(1).head
    val sizeA7 = transA7.sizes(1).head

    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 7) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

    new Kernel7[A1, A2, A3, A4, A5, A6, A7] {
      def apply(a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7): Unit = {
        var bufA1: ByteBuffer = null
        var bufA2: ByteBuffer = null
        var bufA3: ByteBuffer = null
        var bufA4: ByteBuffer = null
        var bufA5: ByteBuffer = null
        var bufA6: ByteBuffer = null
        // val bufA7: ByteBuffer = transA7.toBuffer(a7).head

        var bufA1CLBuf: CLByteBuffer = null
        var bufA2CLBuf: CLByteBuffer = null
        var bufA3CLBuf: CLByteBuffer = null
        var bufA4CLBuf: CLByteBuffer = null
        var bufA5CLBuf: CLByteBuffer = null
        var bufA6CLBuf: CLByteBuffer = null

        time({
          bufA1 = transA1.toBuffer(a1).head
          bufA2 = transA2.toBuffer(a2).head
          bufA3 = transA3.toBuffer(a3).head
          bufA4 = transA4.toBuffer(a4).head
          bufA5 = transA5.toBuffer(a5).head
          bufA6 = transA6.toBuffer(a6).head
          // val bufA7: ByteBuffer = transA7.toBuffer(a7).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
          bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
          bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
          bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA4, true)
          bufA5CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA5, true)
          bufA6CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA6, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val numItemsA3 = transA3.sizes(a3).head / sizeA3
        val numItemsA4 = transA4.sizes(a4).head / sizeA4
        val numItemsA5 = transA5.sizes(a5).head / sizeA5
        val numItemsA6 = transA6.sizes(a6).head / sizeA6
        val numItemsA7 = transA7.sizes(a7).head / sizeA7
        val bufA7capacity = transA7.sizes(a7).head
        val bufA7CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA7capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        time({
          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)
          kernBin.setArg(4, bufA3CLBuf)
          kernBin.setArg(5, numItemsA3)
          kernBin.setArg(6, bufA4CLBuf)
          kernBin.setArg(7, numItemsA4)
          kernBin.setArg(8, bufA5CLBuf)
          kernBin.setArg(9, numItemsA5)
          kernBin.setArg(10, bufA6CLBuf)
          kernBin.setArg(11, numItemsA6)
          kernBin.setArg(12, bufA7CLBuf)
          kernBin.setArg(13, numItemsA7)

          println("Executing with global work size = " + dev.memConfig.globalSize + " and local work size = " + dev.memConfig.localSize)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(14, threads * sizeA1)
            kernBin.setArg(15, threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA2 * threads), Array[Int](threads))
          } else {
            // We don't really know if the local item types are the same as the global item types
            kernBin.setLocalArg(14, dev.memConfig.localMemSize * sizeA1)
            kernBin.setArg(15, dev.memConfig.localMemSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA7capacity)

        time({
          bufA7CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA7.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a7.asInstanceOf[AnyRef], 0, numItemsA7)
        }, "Copy from GPU")
      }
    }
  }

  // ...

  // TODO:
  // Write:
  // object GPUArray {
  //   // This will be compiled into a kernel specialized on f and A and B.
  //   def map(a: BBArray[A], b: BBArray[B], f: A => B) = { ... }
  //   def blockReduce(a: BBArray[A], b: BBArray[B], f: (A,A) => A) = { ... }
  //
  //   def mapKernel(f: A=>B): Kernel2[BBArray[A], BBArray[B]]
  // }
  //
  // class GPUArray[A](a: BBArray[A]) {
  //   def map(f: A => B)(implicit dev: Device) = {
  //     val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[B]) => GPUArray.map(a, b, f) )
  //     val that = BBArray.ofDim[B](a.length)
  //     k(this, that)
  //     new GPUArray(that)
  //   }
  //   def reduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
  //     val that = blockReduce(f)
  //     that.reduceLeft(f)
  //   }
  //   def blockReduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
  //     val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[A]) => GPUArray.blockReduce(a, b, f) )
  //     val that = BBArray.ofDim[B](a.length / blockSize)
  //     k(this, that)
  //     new GPUArray(that)
  //   }
  // }
  //
  // kinda want this:
  // trait Kernel1[A,B] extends Function1[A,B]
  // val k = mapk(_*2) compose reducek(_+_)
  // k(a, b)
  //
  // val a = BBArray.tabulate[Float](1000000)(_.toFloat)
  // val g = GPUArray(a, dev)
  // val b = g.map(_*2).reduce(_+_)
  //
  // g.map returns a MapKernel1
  //
  //
  // val k1 = dev.compile( ... GPUArray.map(.., _*2) )
  // val k2 = dev.compile( ... GPUArray.blockReduce(.., _+_) )
  // 

  /*
  @Deprecated
  def f2bbarrayMapk1[A,B](f: A => B)(implicit ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], dev: Device): BBArrayMapKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel1(f, kernelName)
    println(src)
    implicit val Ma = ma.manifest
    implicit val Mb = mb.manifest
    implicit val ama = implicitly[BBArrayMarshal[A]]
    implicit val amb = implicitly[BBArrayMarshal[B]]
    val kernel = dev.compile1[BBArray[A], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist1[BBArray[A]],
                                                         new SimpleGlobalArrayEffect1[B,BBArray[A]])
    new BBArrayMapKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }

  @Deprecated
  def f2bbarrayMapk2[A1,A2,B](f: (A1,A2) => B)(implicit ma1: FixedSizeMarshal[A1], ma2: FixedSizeMarshal[A2], mb: FixedSizeMarshal[B], dev: Device): BBArrayMapKernel2[A1,A2,B] = {
    val kernelName = freshName("theKernel")
    val src = compileMapKernel2(f, kernelName)
    println(src)
    implicit val Ma1 = ma1.manifest
    implicit val Ma2 = ma2.manifest
    implicit val Mb = mb.manifest
    implicit val ama1 = implicitly[BBArrayMarshal[A1]]
    implicit val ama2 = implicitly[BBArrayMarshal[A2]]
    implicit val amb = implicitly[BBArrayMarshal[B]]
    val kernel = dev.compile2[BBArray[A1], BBArray[A2], BBArray[B]](kernelName, src,
                                                         new SimpleArrayDist2[BBArray[A1], BBArray[A2]],
                                                         new SimpleGlobalArrayEffect2[B,BBArray[A1],BBArray[A2]])
    new BBArrayMapKernel2[A1,A2,B] {
      def apply(a1: BBArray[A1], a2: BBArray[A2]) = kernel(a1, a2)
    }
  }

  @Deprecated
  def f2bbarrayReducek1[A](f: (A,A) => A)(implicit ma: FixedSizeMarshal[A], dev: Device): BBArrayReduceKernel1[A] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    println(src)
    implicit val ama = implicitly[BBArrayMarshal[A]]
    val numThreads = 128 // dev.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val d = new BlockArrayDist1[BBArray[A]](numThreads)
    val e = new SimpleLocalArrayWithOutputEffect1[A,BBArray[A]](numThreads, numThreads * fixedSizeMarshal[A].size)
    val kernel = dev.compile1[BBArray[A], BBArray[A]](kernelName, src, d, e)

    new BBArrayReduceKernel1[A] {
      def apply(a: BBArray[A]) = new Future[A] {

        println(d(a))
        println(e(a))

        lazy val future: Future[BBArray[A]] = kernel(a).start
        def run: Unit = future

        def finish: A = {
          val result = future.force
          println("reduce result = " + result)
          result.reduceLeft(f)
        }
      }
    }
  }

  @Deprecated
  def f2bbarrayLocalReducek1[A,B,L](f: BBArray[A] => GroupIndexed1[B] => (Id1, LocalIndexed1[L]) => Unit)(implicit ml: FixedSizeMarshal[L], ma: FixedSizeMarshal[A], mb: FixedSizeMarshal[B], dev: Device): BBArrayLocalReduceKernel1[A,B] = {
    val kernelName = freshName("theKernel")
    val src = compileReduceKernel1(f, kernelName)
    println(src)
    implicit val ama = implicitly[BBArrayMarshal[A]]
    val numThreads = 128 // dev.device.localMemSize.toInt / 4
    println("numThreads = " + numThreads)
    val d = new BlockArrayDist1[BBArray[A]](numThreads)
    // size of the local buffer is numThreads * sizeof(L)
    val e = new SimpleLocalArrayWithOutputEffect1[B,BBArray[A]](numThreads, numThreads * fixedSizeMarshal[L].size)
    val kernel = dev.compile1[BBArray[A], BBArray[B]](kernelName, src, d, e)
    new BBArrayLocalReduceKernel1[A,B] {
      def apply(a: BBArray[A]) = kernel(a)
    }
  }
  */
}

object Compose {
  /*
  (x,y).zipWith(f).reduce(g)
  =>
  Arg2(x,y).zipWith(f).reduce(g) : Future[B]

  k = zipWith(f).reduce(g): Arg => Future[B]
*/

  val varNames = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def genVarNames(xs: List[_]): List[String] = {
    // TODO: what happens when i >= 52 ?
    xs.indices.toList.map(i => varNames(i).toString)
  }

  // TODO: move to Tree
  object Prototype {
    def apply(typ: Tree, name: Id, formals: List[Tree]): Prototype = Prototype(typ, name.name, formals)
  }
  case class Prototype(typ: Tree, name: String, formals: List[Tree]) extends Tree {
    def toCL = typ.toCL + " " + name + formals.map((t: Tree) => t.toCL).mkString("(", ", ", ");\n\n")
  }

  def compileToTreeName(src: AnyRef, arity: Int): (String, List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply)).reverse
    (methodName(apply), trees)
  }

  def compileToTree(src: AnyRef, arity: Int): (Tree, List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply)).reverse
    (Call(Id(methodName(apply)), (0 until arity).map(i => Id(varNames(i).toString)).toList), trees)
  }

  trait KernelLike {
    def trees: List[Tree]

    lazy val src = header + structs + prototypes + functions + kernelSrc(trees)

    private def header: String = ("\n" +
      "typedef char jbyte;                                  \n" +
      "typedef short jshort;                                \n" +
      "typedef ushort jchar;                                \n" +
      "typedef int jint;                                    \n" +
      "typedef long jlong;                                  \n" +
      "typedef float jfloat;                                \n" +
      "typedef double jdouble;                              \n" +
      "typedef char jboolean;                               \n" +
      "typedef union {                                      \n" +
      "  jbyte b;                                           \n" +
      "  jshort s;                                          \n" +
      "  jchar c;                                           \n" +
      "  jint i;                                            \n" +
      "  jlong l;                                           \n" +
      "  jfloat f;                                          \n" +
      "  jdouble d;                                         \n" +
      "  __global void *gp;                                 \n" +
      "  __local void *lp;                                  \n" +
      "} __any__;                                           \n" +
      "struct Tuple2 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "};                                                   \n" +
      "struct Tuple3 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "};                                                   \n" +
      "struct Tuple4 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "};                                                   \n" +
      "struct Tuple5 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "  __any__ _5;                                        \n" +
      "};                                                   \n" +
      "struct Tuple6 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "  __any__ _5;                                        \n" +
      "  __any__ _6;                                        \n" +
      "};                                                   \n" +
      "\n")

    private def structs = trees.map {
      case t@StructDef(name, fields) => t.toCL + "\n"
      case t => ""
    }.mkString("")

    private def prototypes = trees.map {
      case t@FunDef(returnType, name, formals, _) => Prototype(returnType, name, formals).toCL + "\n"
      case t => ""
    }.mkString("")

    private def functions = trees.map {
      case t@FunDef(_, _, _, _) => t.toCL + "\n\n"
      case t => ""
    }.mkString("")

    protected def kernelSrc(trees: List[Tree]): String
  }
}
