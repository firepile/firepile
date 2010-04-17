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

    val fb = Buffer.make[Float](10)
    println(fb)
    fb.put(3.1415f)
    val f = fb.rewind.get
    println(f)

    val sb = Buffer.make[Short](10)
    println(sb)
    sb.put(2)
    val s = sb.rewind.get
    println(s)

    val ib = Buffer.make[Int](10)
    println(ib)
    ib.put(112)
    val i = ib.rewind.get
    println(i)

    val lb = Buffer.make[Long](10)
    println(lb)
    lb.put(113L)
    val l = lb.rewind.get
    println(l)

    val db = Buffer.make[Double](10)
    println(db)
    db.put(2.71828)
    val d = db.rewind.get
    println(d)

    val bs = Buffer.fromSeq[Byte](List[Byte](1, 2, 3))
    println(bs)
    val e = bs.rewind.get
    println(e)
    
    val cs = Buffer.fromSeq[Char]("abc")
    println(cs)
    val c2 = cs.rewind.get
    println(c2)

    val cs2 = Buffer.fromSeq[Char](List('a', 'b', 'c'))
    println(cs2)
    val c3 = cs2.rewind.get
    println(c3)

    val fs = Buffer.fromSeq[Float](List(1.1f, 2.2f, 3.3f))
    println(fs)
    val f2 = fs.rewind.get
    println(f2)

    val ss = Buffer.fromSeq[Short](List(2, 3, 4))
    println(ss)
    val s2 = ss.rewind.get
    println(s2)

    val is = Buffer.fromSeq[Int](List(5, 6, 7))
    println(is)
    val i2 = is.rewind.get
    println(i2)

    val ls = Buffer.fromSeq[Long](List(8L, 9L, 10L))
    println(ls)
    val l2 = ls.rewind.get
    println(l2)

    val ds = Buffer.fromSeq[Double](List(4.4, 5.5, 6.6))
    println(ds)
    val d2 = ds.rewind.get
    println(d2)

  }
}
