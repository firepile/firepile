
def lerp(value1: Int, value2: Int, ratio: Float) = 
  ((value2 - value1) * ratio + value1).toInt

def lerp(r: Range, ratio: Float) = lerp(r.min, r.max, ratio)

def mapRange(v: Int, old: Range, neu: Range) =
  ((v - old.min) / (old.max - old.min) * (neu.max - neu.min) + neu.min).toInt

def norm(v: Int, min: Int, max: Int) = v.toFloat / (max - min)
def norm(v: Int, r: Range) = v.toFloat / (r.max - r.min)

def constrain(v: Int, min: Int, max: Int) = if (v < min) min else if (v > max) max else v
def constrain(v: Int, r: Range) = if (v < r.min) r.min else if (v > r.max) r.max else v

implicit def wrapRange(r: Range) = new {
  def lerp(ratio: Float) = lerp(r, ratio)
}

implicit def wrapRange(r: Seq[Int]) = new {
  def byfun(f: Int => Int): Iterator[Int] = new Iterator[Int] {
    var n: Option[Int] = if (r isEmpty) None else Some(r.head)
    def hasNext = n match { case None => false case _ => true }
    def next = n match {
      case None => throw new NoSuchElementException
      case Some(x) => {
        val y = f(x)
        n = if (r contains y) Some(y) else None
        x
      }
    }
  }

  def by(f: Int => Int) = byfun(f)
}

Array(1,2,3).indices map (_+1) byfun (_*2) foreach println


trait Array2[A] extends PartialFunction[(Int,Int), A] with Function2[Int,Int,A] {
    def length0: Int
    def length1: Int
    def update(i: Int, j: Int, x: T): Unit
}

object Array2 {
    def fromArray[A](a: Array[A], length0: Int) = new Array2Back1(a, length0)
    def from2dArray[A](a: Array[Array[A]]) = new Array2Back2(a)
}

class Array2Back1[A](a: Array[A], length0: Int) extends PartialFunction[(Int,Int), A] with Function2[Int,Int,A] {
    def apply(i: Int, j: Int) = a(i*length0+j)
    def length1 = a.length / length0
    def update(i: Int, j: Int, x: T) = { a(i*length0+j) = x }
}

implicit def array2array2(a: Array[Array[A]]) = Array2.from2dArray(a)


class Array[A] {
  // The element at given index.
  def apply (i: Int) : T
  // Clone the Array.
  def clone () : Array[T]
  // The length of the array
  def length : Int
  // Update the element at given index.
  def update (i: Int, x: T) : Unit
}
