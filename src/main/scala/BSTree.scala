import com.vividsolutions.jts.geom.Coordinate

/**
 * Created by Fabian on 27-04-15.
 */


sealed trait ArcNode

case class Arc(site: Coordinate, var pred: Option[Arc], var next: Option[Arc], var event: Option[CircleEvent]) extends ArcNode
case class SiteTuple(sites: (Coordinate, Coordinate)) extends ArcNode

class NodeOrdening(y : Int) extends Ordering[ArcNode] {

  implicit def breakPoint(sites : (Coordinate, Coordinate)): Coordinate = {
    // Calculer l'intersection entre la parabole formée par le point a et la droite y, et la parabole formée par le point b et la droite y
    ???
  }
  def compare(a: ArcNode, b : ArcNode): Int = {
    a match {
      case SiteTuple(aSites) => b match {
          case SiteTuple(bSites) => aSites.x.compareTo(bSites.x)
          case Arc(site) => aSites.x.compareTo(site.x)
        }
      case Arc(site) => b match {
          case SiteTuple(bSites) => site.x.compareTo(bSites.x)
          case Arc(p) => site.x.compareTo(p.x)
        }
    }
  }
}

sealed trait BSTree {
  def parent : BSTree
  def toList: List[ArcNode]
  def getLeftMost : Leaf
  def getRightMost : Leaf
}

case class EmptyT() extends BSTree {
  def parent = null
  def toList = Nil
  def getLeftMost = null
  def getRightMost = null
}

case class Leaf(value: Arc, var parent: Node) extends BSTree {
  def toList = Nil
  def getLeftMost = this
  def getRightMost = this
}

case class Node(var left: BSTree, value: SiteTuple, var right: BSTree, var parent: Node) extends BSTree {
  def toList = left.toList ::: value :: right.toList

  def getLeftMost = left.getLeftMost

  def getRightMost = right.getRightMost

}

object Tree {
  def removeArcNode(x: Leaf): BSTree ={
    x match {
      case Leaf(value, parent) if parent == null => {
        val emptyT = new EmptyT()
        emptyT
      }
      case Leaf(valueL, parentL) => {
        parentL match {
          case Node(leftN, valueN, rightN, parentN) if leftN == x => {
            if(parentN == null) rightN
            else {
              parentN match {
                case Node(leftPN, valuePN, rightPN, parentPN) if leftPN == parentL => {
                  parentN.left = rightN
                  parentN //TODO : determine what to return exactly
                }
                case Node(leftPN, valuePN, rightPN, parentPN) => {
                  parentN.right = rightN
                  parentN //TODO : determine what to return exactly

                }
              }
            }
          }
          case Node(leftN, valueN, rightN, parentN) => {
            if(parentN == null) leftN
            else {
              parentN match {
                case Node(leftPN, valuePN, rightPN, parentPN) if leftPN == parentL => {
                  parentN.left = leftN
                  parentN //TODO : determine what to return exactly
                }
                case Node(leftPN, valuePN, rightPN, parentPN) => {
                  parentN.right = leftN
                  parentN //TODO : determine what to return exactly

                }
              }
            }
          }
        }
      }
    }
  }

  def findLeft(x: Node): BSTree = {
    x match {
      case Node(leftN, valueN, rightN, parentN) if parentN != null && parentN.left == x => {
        findLeft(parentN)
      }
      case Node(leftN, valueN, rightN, parentN) if parentN != null && parentN.right == x => {
        parentN
      }
      case Node(leftN, valueN, rightN, parentN) => {
        null
      }
    }
  }

  def findRight(x: Node): BSTree = {
    x match {
      case Node(leftN, valueN, rightN, parentN) if parentN != null && parentN.left == x => {
        parentN
      }
      case Node(leftN, valueN, rightN, parentN) if parentN != null && parentN.right == x => {
        findRight(parentN)
      }
      case Node(leftN, valueN, rightN, parentN) => {
        null
      }
    }
  }

  def insertLeaf(x: Arc, tree: BSTree)(implicit o : Ordering[ArcNode]) : BSTree = {
    import o._
    val newLeaf = Leaf(x, null)
    tree match {
      case Leaf(value, parent) if x < value => {
        val node = Node(newLeaf, SiteTuple((x.site, value.site)), tree, parent)
        newLeaf.parent = node
        node
      }
      case Leaf(value, parent) => {
        val node = Node(tree, SiteTuple((value.site, x.site)), newLeaf, parent)
        newLeaf.parent = node
        node
      }
      case EmptyT() => {
        newLeaf
      }
    }
  }

  def insert(x: Arc, tree: BSTree)(implicit o : Ordering[ArcNode]) : BSTree = {
    import o._
    tree match {
      case t : Leaf => insertLeaf(x, t)
      case Node(left, value, right, parent) if x < value => {
        Node(insert(x, left), value, right, parent)
      }
      case Node(left, value, right, parent) => {
        Node(left, value, insert(x, right), parent)
      }
    }
  }


  def replaceNode(oldNode: Leaf, newNode: Node, tree: BSTree) : BSTree = {
    val left = newNode.getLeftMost
    val right = newNode.getRightMost
    val pred = oldNode.value.pred
    val next = oldNode.value.next

    //Update Links
    left.value.pred = pred
    right.value.next = next
    pred.foreach(_.next = Some(left.value))
    next.foreach(_.pred = Some(right.value))


    //Update the tuples



  }

  def insertNode(x: SiteTuple, tree: BSTree)(implicit o : Ordering[ArcNode]): BSTree = {
    import o._
    //TODO
    tree match {
      case Node(left, value, right, parent) if x < value =>
        Node(insert(x, left), value, right, parent)
      case Node(left, value, right, parent) =>
        Node(left, value, insert(x, right), parent)
    }
  }

  def search(x: Arc, tree: BSTree)(implicit o : Ordering[ArcNode]): Leaf = {
    import o._
    tree match {
      case Leaf => false
      case Node(left, value, right) if x < value => search(x, left)
      case Node(left, value, right) if x > value => search(x, right)
      case _ => true
    }
  }

  def search(x: Arc, tree: BSTree)(implicit o : Ordering[ArcNode]): Leaf = {
    import o._
    ???
  }

  def tsort[T <% Ordered[T]](values: List[T]) = values.foldRight(Leaf: BSTree[T])(insert).toList
}