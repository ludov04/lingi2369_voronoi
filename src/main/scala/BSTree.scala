/**
 * Created by Fabian on 27-04-15.
 */
sealed trait BSTree[+T] {
  def toList: List[T]
}

case class Leaf[T](value: T, parent: BSTree[T]) extends BSTree[T] {
  def toList = Nil
}

case class Node[T](left: BSTree[T], value: T, right: BSTree[T], parent: BSTree[T]) extends BSTree[T] {
  def toList = left.toList ::: value :: right.toList
}

object Tree {
  def insert[T <% Ordered[T]](x: T, tree: BSTree[T]): BSTree[T] = tree match {
    case Leaf(value) if x < value => Node(Leaf, x, Leaf, null)
    case Node(left, value, right) if x < value =>
      Node(insert(x, left), value, right)
    case Node(left, value, right) =>
      Node(left, value, insert(x, right))
  }

  def search[T <% Ordered[T]](x: T, tree: BSTree[T]): Boolean = tree match {
    case Leaf => false
    case Node(left, value, right) if x < value => search(x, left)
    case Node(left, value, right) if x > value => search(x, right)
    case _ => true
  }

  def tsort[T <% Ordered[T]](values: List[T]) = values.foldRight(Leaf: BSTree[T])(insert).toList
}