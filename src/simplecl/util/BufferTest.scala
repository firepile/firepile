package simplecl.util

object BufferTest {
  def main(args: Array[String]) = {
    val bb = Buffer.make[Byte](10)
    println(bb)
    bb.put(10)
    val b = bb.rewind.get
    println(b)

    val cb = Buffer.make[Char](10)
    println(cb)
    cb.put('a')
    val c = cb.rewind.get
    println(c)

    val cs = Buffer.fromSeq[Char]("abc")
    println(cs)
    val d = cs.rewind.get
    println(d)

    val cs2 = Buffer.fromSeq[Char](List('a', 'b', 'c'))
    println(cs2)
    val d2 = cs2.rewind.get
    println(d2)


    val bs = Buffer.fromSeq[Byte](List[Byte](1, 2, 3))
    println(bs)
    val e = bs.rewind.get
    println(e)
  }
}
