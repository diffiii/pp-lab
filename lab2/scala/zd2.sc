// Zadanie 2

def dlugosc[A](list: List[A]): Int = {
  list match {
    case Nil => 0
    case _ :: tail => 1 + dlugosc(tail)
  }
}

Console.println(dlugosc(List(5, 4, 3, 2)))
Console.println(dlugosc(List(1, 1, 1, 1, 1, 1)))
Console.println(dlugosc(List(5, 4)))
Console.println(dlugosc(List(3)))
Console.println(dlugosc(List()))
