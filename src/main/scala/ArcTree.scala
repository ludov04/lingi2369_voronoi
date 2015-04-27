/**
 * Created by Fabian on 27-04-15.
 */
abstract class ArcTree[+A <% Ordered[A], B <% A] {
  def value: Either[A, B]

  def left: ArcTree[A, B]

  def right: ArcTree[A, B]

  def isEmpty: Boolean

  def parent: ArcTree[A, B]

  def isLeaf: Boolean

  def add[C >: A <% Ordered[C]](x: C): ArcTree[C, B] = {

  }
}

case class Branch[B](value: B,
                     left: ArcTree[C, B],
                     right: ArcTree[C, B]) extends ArcTree[A, B] {
  def isEmpty = false
}