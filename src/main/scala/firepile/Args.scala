package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.Spaces._
import firepile.tree.Trees._

import compiler.JVM2CL.compileRoot
import compiler.JVM2CL.mangleName
import compiler.JVM2CL.methodName

import firepile.Implicits._
import java.nio.ByteBuffer

object Args {
  import Compose._

  class ArgHasLength[A,X<:Arg[A,X]] extends HasLength[X] {
    def length(a: X) = a.length
  }
  implicit def ahl[A,X<:Arg[A,X]] = new ArgHasLength[A,X]

  implicit def arg2array[A](a: Arg[A,_]) = a.value

  abstract class ArgMarshal[ArgA <: Arg[_,_]] extends Marshal[ArgA] {
    def sizes(a: ArgA) = sizes(a.length)
    def sizes(len: Int): List[Int]
    def align: Int
    def toBuffer(a: ArgA) = a.buffers
  }

  class Arg1Marshal[A:FixedSizeMarshal] extends ArgMarshal[Arg1[A]] {
    def sizes(len: Int) = (len * implicitly[FixedSizeMarshal[A]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A]].align
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b :: Nil => new Arg1(new BBArray[A](b))
      case _ => throw new MatchError
    }
  }

  class Arg2Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal] extends ArgMarshal[Arg2[A1,A2]] {
    def sizes(len: Int) = 
        (len * implicitly[FixedSizeMarshal[A1]].size) ::
        (len * implicitly[FixedSizeMarshal[A2]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A1]].align max implicitly[FixedSizeMarshal[A2]].align
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b1 :: b2 :: Nil => new Arg2(new BBArray[A1](b1), new BBArray[A2](b2))
      case _ => throw new MatchError
    }
  }

  class Arg3Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal] extends ArgMarshal[Arg3[A1,A2,A3]] {
    def sizes(len: Int) = 
        (len * implicitly[FixedSizeMarshal[A1]].size) ::
        (len * implicitly[FixedSizeMarshal[A2]].size) ::
        (len * implicitly[FixedSizeMarshal[A3]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A1]].align max implicitly[FixedSizeMarshal[A2]].align max
                     implicitly[FixedSizeMarshal[A3]].align
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b1 :: b2 :: b3 :: Nil => new Arg3(new BBArray[A1](b1), new BBArray[A2](b2), new BBArray[A3](b3))
      case _ => throw new MatchError
    }
  }

  class Arg4Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal] extends ArgMarshal[Arg4[A1,A2,A3,A4]] {
    def sizes(len: Int) = 
        (len * implicitly[FixedSizeMarshal[A1]].size) ::
        (len * implicitly[FixedSizeMarshal[A2]].size) ::
        (len * implicitly[FixedSizeMarshal[A3]].size) ::
        (len * implicitly[FixedSizeMarshal[A4]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A1]].align max implicitly[FixedSizeMarshal[A2]].align max
                     implicitly[FixedSizeMarshal[A3]].align max implicitly[FixedSizeMarshal[A4]].align
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b1 :: b2 :: b3 :: b4 :: Nil => new Arg4(new BBArray[A1](b1), new BBArray[A2](b2), new BBArray[A3](b3), new BBArray[A4](b4))
      case _ => throw new MatchError
    }
  }

  class Arg5Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal] extends ArgMarshal[Arg5[A1,A2,A3,A4,A5]] {
    def sizes(len: Int) = 
        (len * implicitly[FixedSizeMarshal[A1]].size) ::
        (len * implicitly[FixedSizeMarshal[A2]].size) ::
        (len * implicitly[FixedSizeMarshal[A3]].size) ::
        (len * implicitly[FixedSizeMarshal[A4]].size) ::
        (len * implicitly[FixedSizeMarshal[A5]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A1]].align max implicitly[FixedSizeMarshal[A2]].align max
                     implicitly[FixedSizeMarshal[A3]].align max implicitly[FixedSizeMarshal[A4]].align max
                     implicitly[FixedSizeMarshal[A5]].align
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: Nil => new Arg5(new BBArray[A1](b1), new BBArray[A2](b2), new BBArray[A3](b3), new BBArray[A4](b4), new BBArray[A5](b5))
      case _ => throw new MatchError
    }
  }

  class Arg6Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal,A6:FixedSizeMarshal] extends ArgMarshal[Arg6[A1,A2,A3,A4,A5,A6]] {
    def sizes(len: Int) = 
        (len * implicitly[FixedSizeMarshal[A1]].size) ::
        (len * implicitly[FixedSizeMarshal[A2]].size) ::
        (len * implicitly[FixedSizeMarshal[A3]].size) ::
        (len * implicitly[FixedSizeMarshal[A4]].size) ::
        (len * implicitly[FixedSizeMarshal[A5]].size) ::
        (len * implicitly[FixedSizeMarshal[A6]].size) :: Nil
    def align: Int = implicitly[FixedSizeMarshal[A1]].align max implicitly[FixedSizeMarshal[A2]].align max
                     implicitly[FixedSizeMarshal[A3]].align max implicitly[FixedSizeMarshal[A4]].align max
                     implicitly[FixedSizeMarshal[A5]].align max implicitly[FixedSizeMarshal[A6]].align
    def fromBuffer(bs: List[ByteBuffer]) = bs match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: Nil => new Arg6(new BBArray[A1](b1), new BBArray[A2](b2), new BBArray[A3](b3), new BBArray[A4](b4), new BBArray[A5](b5), new BBArray[A6](b6))
      case _ => throw new MatchError
    }
  }

  implicit def Arg1Marshal[A:FixedSizeMarshal] = new Arg1Marshal[A]
  implicit def Arg2Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal] = new Arg2Marshal[A1,A2]
  implicit def Arg3Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal] = new Arg3Marshal[A1,A2,A3]
  implicit def Arg4Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal] = new Arg4Marshal[A1,A2,A3,A4]
  implicit def Arg5Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal] = new Arg5Marshal[A1,A2,A3,A4,A5]
  implicit def Arg6Marshal[A1:FixedSizeMarshal,A2:FixedSizeMarshal,A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal,A6:FixedSizeMarshal] = new Arg6Marshal[A1,A2,A3,A4,A5,A6]

  sealed abstract class Arg[A: FixedSizeMarshal,ArgA<:Arg[A,ArgA]] {
    this: ArgA =>
    def mapk[B,ArgB<:Arg[B,ArgB]](m: Mapper[A,B,ArgA,ArgB])(implicit dev: Device, mb: FixedSizeMarshal[B]) = new MapKernel[A,B,ArgA,ArgB](dev, m.trees, m.mapTree, m.builder, m.mab)
    def length: Int
    def arity: Int
    def buffers: List[ByteBuffer]
    def reduce(f: (A,A) => A) = value.reduceLeft(f)
    def value: BBArray[A]
  }

  abstract class Argn[A:FixedSizeMarshal,ArgA<:Arg[A,ArgA]] extends Arg[A,ArgA] {
    this: ArgA =>
    def zipWith[B,ArgB<:Arg[B,ArgB]](m: Mapper[A,B,ArgA,ArgB])(implicit dev: Device, mb: FixedSizeMarshal[B]) = new MapKernel[A,B,ArgA,ArgB](dev, m.trees, m.mapTree, m.builder, m.mab)
  }

  def idMapper[A:FixedSizeMarshal]: Mapper[A,A,Arg1[A],Arg1[A]] = f2Mapper[A,A]((x:A) => x)

  class Arg1[A:FixedSizeMarshal](a: BBArray[A]) extends Arg[A,Arg1[A]] {
    lazy val m = idMapper[A]
    def reduceBlock(r: Reducer[A])(implicit dev: Device) = new MapBlockReduceKernel[A,A,Arg1[A],Arg1[A]](dev, m.trees, r.trees, m.mapTree, r.reduceTree, m.builder, m.mab)
    def reduce(r: Reducer[A])(implicit dev: Device) = reduceBlock(r)(dev).reduce(r.reduceFun)
    def length = a.length
    def arity = 1
    def buffers = a.buffer :: Nil
    def value = a
  }
  class Arg2[A1:FixedSizeMarshal, A2:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2]) extends Argn[(A1,A2),Arg2[A1,A2]] {
    assert(a1.length == a2.length)
    def length = a1.length
    def arity = 2
    def buffers = a1.buffer :: a2.buffer :: Nil
    def value = a1 zip a2
  }
  class Arg3[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2], a3: BBArray[A3]) extends Argn[(A1,A2,A3),Arg3[A1,A2,A3]] {
    assert(a1.length == a2.length)
    assert(a1.length == a3.length)
    def length = a1.length
    def arity = 3
    def buffers = a1.buffer :: a2.buffer :: a3.buffer :: Nil
    def value = BBArray.fromArray((0 until a1.length).map(i => (a1(i),a2(i),a3(i))).toArray)
  }
  class Arg4[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal, A4:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2], a3: BBArray[A3], a4: BBArray[A4]) extends Argn[(A1,A2,A3,A4),Arg4[A1,A2,A3,A4]] {
    assert(a1.length == a2.length)
    assert(a1.length == a3.length)
    assert(a1.length == a4.length)
    def length = a1.length
    def arity = 4
    def buffers = a1.buffer :: a2.buffer :: a3.buffer :: a4.buffer :: Nil
    def value = BBArray.fromArray((0 until a1.length).map(i => (a1(i),a2(i),a3(i),a4(i))).toArray)
  }
  class Arg5[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2], a3: BBArray[A3], a4: BBArray[A4], a5: BBArray[A5]) extends Argn[(A1,A2,A3,A4,A5),Arg5[A1,A2,A3,A4,A5]] {
    assert(a1.length == a2.length)
    assert(a1.length == a3.length)
    assert(a1.length == a4.length)
    assert(a1.length == a5.length)
    def length = a1.length
    def arity = 5
    def buffers = a1.buffer :: a2.buffer :: a3.buffer :: a4.buffer :: a5.buffer :: Nil
    def value = BBArray.fromArray((0 until a1.length).map(i => (a1(i),a2(i),a3(i),a4(i),a5(i))).toArray)
  }
  class Arg6[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal,A4:FixedSizeMarshal,A5:FixedSizeMarshal,A6:FixedSizeMarshal](a1: BBArray[A1], a2: BBArray[A2], a3: BBArray[A3], a4: BBArray[A4], a5: BBArray[A5], a6: BBArray[A6]) extends Argn[(A1,A2,A3,A4,A5,A6),Arg6[A1,A2,A3,A4,A5,A6]] {
    assert(a1.length == a2.length)
    assert(a1.length == a3.length)
    assert(a1.length == a4.length)
    assert(a1.length == a5.length)
    assert(a1.length == a6.length)
    def length = a1.length
    def arity = 6
    def buffers = a1.buffer :: a2.buffer :: a3.buffer :: a4.buffer :: a5.buffer :: a6.buffer :: Nil
    def value = BBArray.fromArray((0 until a1.length).map(i => (a1(i),a2(i),a3(i),a4(i),a5(i),a6(i))).toArray)
  }

  implicit def Arg1[A:FixedSizeMarshal](a: BBArray[A]) = new Arg1(a)
  implicit def Arg2[A1:FixedSizeMarshal, A2:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2])) = new Arg2(p._1, p._2)
  implicit def Arg3[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2], BBArray[A3])) = new Arg3(p._1, p._2, p._3)
  implicit def Arg4[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal, A4:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2], BBArray[A3], BBArray[A4])) = new Arg4(p._1, p._2, p._3, p._4)
  implicit def Arg5[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal, A4:FixedSizeMarshal, A5:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2], BBArray[A3], BBArray[A4], BBArray[A5])) = new Arg5(p._1, p._2, p._3, p._4, p._5)
  implicit def Arg6[A1:FixedSizeMarshal, A2:FixedSizeMarshal, A3:FixedSizeMarshal, A4:FixedSizeMarshal, A5:FixedSizeMarshal, A6:FixedSizeMarshal](p: (BBArray[A1], BBArray[A2], BBArray[A3], BBArray[A4], BBArray[A5], BBArray[A6])) = new Arg6(p._1, p._2, p._3, p._4, p._5, p._6)
}
