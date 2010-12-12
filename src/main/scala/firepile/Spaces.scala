package firepile

import util.BufferBackedArray._
import firepile.Marshaling._

object Spaces {

  object localMem { def barrier = () }

  trait IdSpace[Pt <: Point[Pt]] extends Iterable[Pt] {
    def extent: Pt
    def length: Int
    def index(p: Pt): Int
  }
  class IdSpace1(val extent: Point1) extends IdSpace[Point1] {
    def iterator = (for (i <- 0 until extent.x) yield Point1(i)).toIterator
    def length = extent.x
    def index(p: Point1) = {
      val q = p | this
      q.x
    }
  }
  class IdSpace2(val extent: Point2) extends IdSpace[Point2] {
    def iterator = (for (i1 <- 0 until extent.x; i2 <- 0 until extent.y) yield Point2(i1, i2)).toIterator
    def length = extent.x * extent.y
    def index(p: Point2) = {
      val q = p | this
      q.x * extent.x + q.y
    }
  }
  class IdSpace3(val extent: Point3) extends IdSpace[Point3] {
    def iterator = (for (i1 <- 0 until extent.x; i2 <- 0 until extent.y; i3 <- 0 until extent.z) yield Point3(i1, i2, i3)).toIterator
    def length = extent.x * extent.y * extent.z
    def index(p: Point3) = {
      val q = p | this
      (q.x * extent.x + q.y) * extent.y + q.z
    }
  }

  trait Point[Pt <: Point[Pt]] {
    this: Pt =>

    def rank: Int

    def +(i: Int): Pt
    def -(i: Int): Pt
    def /(i: Int): Pt
    def %(i: Int): Pt
    def *(i: Int): Pt

    def +(p: Pt): Pt
    def -(p: Pt): Pt
    def *(p: Pt): Pt
    def /(p: Pt): Pt
    def %(p: Pt): Pt

    def scale(b: Pt, n: Pt, t: Pt): Pt = b * n + t

    def |(s: IdSpace[Pt]): Pt
  }
  case class Point1(x: Int) extends Point[Point1] {
    def rank = 1

    def toInt = x

    def +(i: Int) = Point1(x+i)
    def -(i: Int) = Point1(x-i)
    def /(i: Int) = Point1(x/i)
    def %(i: Int) = Point1(x%i)
    def *(i: Int) = Point1(x*i)

    def +(p: Point1) = Point1(x+p.x)
    def -(p: Point1) = Point1(x-p.x)
    def *(p: Point1) = Point1(x*p.x)
    def /(p: Point1) = Point1(x/p.x)
    def %(p: Point1) = Point1(x%p.x)

    def |(s: IdSpace[Point1]) = Point1(x % s.extent.x)
  }
  case class Point2(x: Int, y: Int) extends Point[Point2] {
    def rank = 2

    def toTuple2 = (x,y)

    def +(i: Int) = Point2(x+i,y+i)
    def -(i: Int) = Point2(x-i,y-i)
    def /(i: Int) = Point2(x/i, y/i)
    def %(i: Int) = Point2(x%i, y%i)
    def *(i: Int) = Point2(x*i, y*i)

    def +(i: Int, j: Int) = Point2(x+i,y+j)
    def -(i: Int, j: Int) = Point2(x-i,y-j)
    def *(i: Int, j: Int) = Point2(x*i,y*j)
    def /(i: Int, j: Int) = Point2(x/i,y/j)
    def %(i: Int, j: Int) = Point2(x%i,y%j)

    def +(p: Point2) = Point2(x+p.x,y+p.y)
    def -(p: Point2) = Point2(x-p.x,y-p.y)
    def *(p: Point2) = Point2(x*p.x, y*p.y)
    def /(p: Point2) = Point2(x/p.x, y/p.y)
    def %(p: Point2) = Point2(x%p.x, y%p.y)

    def |(s: IdSpace[Point2]) = Point2(x % s.extent.x, y % s.extent.y)
  }
  case class Point3(x: Int, y: Int, z: Int) extends Point[Point3] {
    def rank = 3

    def toTuple3 = (x,y,z)

    def +(i: Int) = Point3(x+i,y+i,z+i)
    def -(i: Int) = Point3(x-i,y-i,z-i)
    def /(i: Int) = Point3(x/i, y/i, z/i)
    def %(i: Int) = Point3(x%i, y%i, z%i)
    def *(i: Int) = Point3(x*i, y*i, z*i)

    def +(i: Int, j: Int, k: Int) = Point3(x+i,y+j,z+k)
    def -(i: Int, j: Int, k: Int) = Point3(x-i,y-j,z-k)
    def *(i: Int, j: Int, k: Int) = Point3(x*i,y*j,z*k)
    def /(i: Int, j: Int, k: Int) = Point3(x/i,y/j,z/k)
    def %(i: Int, j: Int, k: Int) = Point3(x%i,y%j,z%k)

    def +(p: Point3) = Point3(x+p.x,y+p.y,z+p.z)
    def -(p: Point3) = Point3(x-p.x,y-p.y,z-p.z)
    def *(p: Point3) = Point3(x*p.x,y*p.y,z*p.z)
    def /(p: Point3) = Point3(x/p.x,y/p.y,z/p.z)
    def %(p: Point3) = Point3(x%p.x,y%p.y,z%p.z)

    def |(s: IdSpace[Point3]) = Point3(x % s.extent.x, y % s.extent.y, z % s.extent.z)
  }

  implicit def int2point1(p: Int) = Point1(p)
  implicit def int2point2(p: (Int,Int)) = Point2(p._1,p._2)
  implicit def int2point3(p: (Int,Int,Int)) = Point3(p._1,p._2,p._3)
  implicit def point12int(p: Point1) = p.x
  implicit def point22int(p: Point2) = (p.x,p.y)
  implicit def point32int(p: Point3) = (p.x,p.y,p.z)

  class Ident[Pt <: Point[Pt]](val config: Config[Pt], val group: Pt, val local: Pt) {
    def thread: Pt = group * config.localIdSpace.extent + local
    def global = thread
  }

  class Id1(config: Config1, group: Point1, local: Point1) extends Ident[Point1](config, group, local)
  class Id2(config: Config2, group: Point2, local: Point2) extends Ident[Point2](config, group, local)
  class Id3(config: Config3, group: Point3, local: Point3) extends Ident[Point3](config, group, local)

  /** Index space */
  trait Config[Pt <: Point[Pt]] {
    /** Space of global thread ids; must be groupIdSpace * localIdSpace */
    def threadIdSpace: IdSpace[Pt]
    /** Space of group (work group) ids */
    def groupIdSpace: IdSpace[Pt]
    /** Space of local thread (work item) ids witin a group.  All groups have the same local space. */
    def localIdSpace: IdSpace[Pt]

    def threadIds = threadIdSpace.iterator
    def groupIds = groupIdSpace.iterator
    def localIds = localIdSpace.iterator

    /** Return the group ID of a given global thread ID */
    def groupIdOfThread(p: Pt) = p / groupIdSpace.extent
    def localIdOfThread(p: Pt) = p % groupIdSpace.extent

    def numThreads = threadIdSpace.length
    def numGroups = groupIdSpace.length
    def numThreadsPerGroup = localIdSpace.length

    def groupSize = numThreadsPerGroup
    def localSize = numThreadsPerGroup
    def globalSize = numThreads
    def gridSize = numThreads
  }

  class Config1(val maxGroup: Point1, val maxLocal: Point1) extends Config[Point1] {
    type Pt = Point1
    def threadIdSpace = new IdSpace1(maxGroup * maxLocal)
    def groupIdSpace = new IdSpace1(maxGroup)
    def localIdSpace = new IdSpace1(maxLocal)
  }
  class Config2(val maxGroup: Point2, val maxLocal: Point2) extends Config[Point2] {
    type Pt = Point2
    def threadIdSpace = new IdSpace2(maxGroup * maxLocal)
    def groupIdSpace = new IdSpace2(maxGroup)
    def localIdSpace = new IdSpace2(maxLocal)
  }
  class Config3(val maxGroup: Point3, val maxLocal: Point3) extends Config[Point3] {
    type Pt = Point3
    def threadIdSpace = new IdSpace3(maxGroup * maxLocal)
    def groupIdSpace = new IdSpace3(maxGroup)
    def localIdSpace = new IdSpace3(maxLocal)
  }

  class Indexed[Pt <: Point[Pt], A: FixedSizeMarshal](space: IdSpace[Pt]) {
    private[firepile] val backing: BBArray[A] = BBArray.ofDim[A](space.length)
    def apply(p: Pt): A = backing(space.index(p))
    def update(p: Pt, x: A): Unit = { backing(space.index(p)) = x }
  }
  class LocalIndexed[Pt <: Point[Pt], A: FixedSizeMarshal](config: Config[Pt]) extends Indexed[Pt,A](config.localIdSpace)
  class LocalIndexed1[A: FixedSizeMarshal](config: Config1) extends LocalIndexed[Point1, A](config)
  class LocalIndexed2[A: FixedSizeMarshal](config: Config2) extends LocalIndexed[Point2, A](config)
  class LocalIndexed3[A: FixedSizeMarshal](config: Config3) extends LocalIndexed[Point3, A](config)

  class GroupIndexed[Pt <: Point[Pt], A: FixedSizeMarshal](config: Config[Pt]) extends Indexed[Pt,A](config.groupIdSpace)
  class GroupIndexed1[A: FixedSizeMarshal](config: Config1) extends GroupIndexed[Point1, A](config)
  class GroupIndexed2[A: FixedSizeMarshal](config: Config2) extends GroupIndexed[Point2, A](config)
  class GroupIndexed3[A: FixedSizeMarshal](config: Config3) extends GroupIndexed[Point3, A](config)

  class ThreadIndexed[Pt <: Point[Pt], A: FixedSizeMarshal](config: Config[Pt]) extends Indexed[Pt,A](config.threadIdSpace)
  class ThreadIndexed1[A: FixedSizeMarshal](config: Config1) extends ThreadIndexed[Point1, A](config)
  class ThreadIndexed2[A: FixedSizeMarshal](config: Config2) extends ThreadIndexed[Point2, A](config)
  class ThreadIndexed3[A: FixedSizeMarshal](config: Config3) extends ThreadIndexed[Point3, A](config)

  import firepile.util.BufferBackedArray._
  import java.nio.ByteBuffer

  class Point1Marshal[A] extends FixedSizeMarshal[Point1] {
    def size = fixedSizeMarshal[Int].size
    def align = fixedSizeMarshal[Int].align
    override def put(buf:ByteBuffer, i: Int, p: Point1) = {
      fixedSizeMarshal[Int].put(buf, i, p.x)
    }
    override def get(buf:ByteBuffer, i: Int) = {
      val x = fixedSizeMarshal[Int].get(buf, i)
      Point1(x)
    }
    val manifest = classManifest[Point1]
  }
  class Point2Marshal[A] extends FixedSizeMarshal[Point2] {
    def size = fixedSizeMarshal[Int].size * 2
    def align = fixedSizeMarshal[Int].align
    override def put(buf:ByteBuffer, i: Int, p: Point2) = {
      fixedSizeMarshal[Int].put(buf, i, p.x)
      fixedSizeMarshal[Int].put(buf, i, p.y)
    }
    override def get(buf:ByteBuffer, i: Int) = {
      val x = fixedSizeMarshal[Int].get(buf, i)
      val y = fixedSizeMarshal[Int].get(buf, i+fixedSizeMarshal[Int].size)
      Point2(x, y)
    }
    val manifest = classManifest[Point2]
  }
  class Point3Marshal[A] extends FixedSizeMarshal[Point3] {
    def size = fixedSizeMarshal[Int].size * 3
    def align = fixedSizeMarshal[Int].align
    override def put(buf:ByteBuffer, i: Int, p: Point3) = {
      fixedSizeMarshal[Int].put(buf, i, p.x)
      fixedSizeMarshal[Int].put(buf, i+fixedSizeMarshal[Int].size, p.y)
      fixedSizeMarshal[Int].put(buf, i+2*fixedSizeMarshal[Int].size, p.z)
    }
    override def get(buf:ByteBuffer, i: Int) = {
      val x = fixedSizeMarshal[Int].get(buf, i)
      val y = fixedSizeMarshal[Int].get(buf, i+fixedSizeMarshal[Int].size)
      val z = fixedSizeMarshal[Int].get(buf, i+2*fixedSizeMarshal[Int].size)
      Point3(x, y, z)
    }
    val manifest = classManifest[Point3]
  }
}
