def skip[A](stream: Stream[A], m: Int): Stream[A] = {
  if (m <= 0) stream
  else
    stream match {
      case _ #:: xs => skip(xs, m - 1)
      case _        => Stream.empty
    }
}

def lchoose[A](stream: Stream[A], n: Int, m: Int): Stream[A] = {
  if (n < 0) throw new IllegalArgumentException("n must be greater than 0")
  if (m < 0) throw new IllegalArgumentException("m must be greater than 0")
  skip(stream, m - 1) match {
    case x #:: xs => x #:: lchoose(xs, n, n)
    case _        => Stream.empty
  }
}

Console.println(lchoose(Stream(5, 6, 3, 2, 1), 2, 1).take(10).toList)
Console.println(lchoose(Stream.from(1), 1, 2).take(10).toList)
Console.println(lchoose(Stream(0), 1, 1).take(10).toList)
Console.println(lchoose(null, 1, 2).take(10).toList)
Console.println(lchoose(Stream(1, 2, 3), 0, -1).take(10).toList)
Console.println(lchoose(Stream(1, 2, 3), 1, 5).take(10).toList)
