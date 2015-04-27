/**
 * Created by Fabian on 27-04-15.
 */
abstract class ArcTree[+A <% Ordered[A], B <% A] {
  def value: Either[A, B]

  def left: ArcTree[A, B]

  def right: ArcTree[A, B]

  def isEmpty: Boolean

  def parent: Option[ArcTree[A, B]]

  def isLeaf: Boolean

  def add[C >: A <% Ordered[C]](x: C): ArcTree[C, B] = {
    if(this.isEmpty) Leaf(Left(x), None)
    else if(this.isLeaf){
      ???
    } else if(x < this.value.right.get){
      ???
    } else if(x >= this.value.right.get){
      ???
    }
  }
}

case class Branch[+A <% Ordered[A], B <% A](value: Right[A, B],
                                            left: ArcTree[A, B],
                                            right: ArcTree[A, B],
                                            parent: Option[ArcTree[A, B]] ) extends ArcTree[A, B] {
  def isEmpty = false

  def isLeaf = false
}

/*case class Leaf[+A <% Ordered[A], B <% A](value: Left[A, B],
                                          parent: Option[ArcTree[A, B]] ) extends ArcTree[A, B] {

  def left: ArcTree[Nothing, Nothing] = throw new NoSuchElementException("An empty tree.")

  def right: ArcTree[Nothing, Nothing] = throw new NoSuchElementException("An empty tree.")

  def isEmpty = false

  def isLeaf = false
}*/