/* Zadanie 2 */

sealed trait BinaryTree[A]
case class Empty[A]() extends BinaryTree[A]
case class Node[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]


def tree_my(tree: BinaryTree[Int], function: Int => Int): Int = {
  @annotation.tailrec
  def tree_my_rec(trees: List[BinaryTree[Int]], acc: Int): Int = {
    trees match {
      case Nil => acc
      case Empty() :: rest => tree_my_rec(rest, acc)
      case Node(value, left, right) :: rest =>
        tree_my_rec(left :: right :: rest, acc * function(value))
    }
  }

  if tree == null then throw new IllegalArgumentException("Tree is null")
  if tree == Empty[Int]() then 0
  else tree_my_rec(List(tree), 1)
}

def tree_my_sq(tree: BinaryTree[Int]): Int = tree_my(tree, x => x * x)



Console.println(tree_my_sq(Node(1,
  Node(2, Node(4, Empty[Int](), Empty[Int]()), Empty[Int]()),
  Node(3, Node(5, Empty[Int](), Node(6, Empty[Int](), Empty[Int]())), Empty[Int]())
)))

Console.println(tree_my_sq(Node(10,
  Node(10, Empty[Int](), Empty[Int]()),
  Node(10, Empty[Int](), Empty[Int]())
)))

Console.println(tree_my_sq(Node(1,
  Node(1, Node(1, Empty[Int](), Empty[Int]()), Node(1, Empty[Int](), Empty[Int]())),
  Node(1, Node(1, Empty[Int](), Empty[Int]()), Node(1, Empty[Int](), Empty[Int]()))
)))

Console.println(tree_my_sq(Empty[Int]()))

Console.println(tree_my_sq(null))
