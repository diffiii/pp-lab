/* Zadanie 2 */

def sum(func: Int => Int, a: Int, b: Int): Int = {
  @annotation.tailrec
  def sumRec(n: Int, acc: Int): Int = {
    if (n > b) acc
    else sumRec(n + 1, acc + func(n))
  }

  sumRec(a, 0)
}


def funcId(x: Int): Int = x

def funcSquare(x: Int): Int = x * x

def funcFactorial(x: Int): Int = {
  @annotation.tailrec
  def factorialRec(n: Int, acc: Int): Int = {
    if (n < 0) throw new IllegalArgumentException("Negative value")
    else if (n <= 1) acc
    else factorialRec(n - 1, acc * n)
  }

  factorialRec(x, 0)
}



Console.println(sum(funcId, 1, 3))
Console.println(sum(funcId, 0, 10))
Console.println(sum(funcId, -5, -2))
Console.println(sum(funcId, 4, 2))

Console.println(sum(funcSquare, 1, 3))
Console.println(sum(funcSquare, 0, 10))
Console.println(sum(funcSquare, -5, -2))
Console.println(sum(funcSquare, 4, 2))

Console.println(sum(funcFactorial, 1, 3))
Console.println(sum(funcFactorial, 0, 10))
Console.println(sum(funcFactorial, -5, -2))
Console.println(sum(funcFactorial, 4, 2))
