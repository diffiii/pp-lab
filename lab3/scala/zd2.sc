/* Zadanie 2 */

def sum(func: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else func(a) + sum(func, a + 1, b)
}


def funcId(x: Int): Int = x

def funcSquare(x: Int): Int = x * x

def funcFactorial(x: Int): Int = {
  if (x < 0) 0
  else if (x <= 1) 1
  else x * funcFactorial(x - 1)
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
