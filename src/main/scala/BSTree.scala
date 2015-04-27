import com.vividsolutions.jts.geom.Coordinate

/**
 * Created by Fabian on 27-04-15.
 */


sealed trait ArcNode

case class Arc(site: Coordinate, pred: Option[Arc], next: Option[Arc], event: Option[CircleEvent]) extends ArcNode
case class SiteTuple(sites: (Coordinate, Coordinate)) extends ArcNode


class NodeOrdening(y : Int) extends Ordering[ArcNode] {

  implicit def breakPoint(sites : (Coordinate, Coordinate)): Coordinate = {
    // Calculer l'intersection entre la parabole formée par le point a et la droite y, et la parabole formée par le point b et la droite y
    ???
  }
  def compare(a: ArcNode, b : ArcNode): Int = {
    a match {
      case SiteTuple(aSites) => {
        b match {
          case SiteTuple(bSites) => {
            aSites.x.compareTo(bSites.x)
          }
          case Arc(site) => {
            aSites.x.compareTo(site.x)
          }
        }

      }
      case Arc(site) => {
        b match {
          case SiteTuple(bSites) => {
            site.x.compareTo(bSites.x)
          }
          case Arc(p) => {
            site.x.compareTo(p.x)
          }
        }
      }
    }
  }
}

sealed trait BSTree {
  def toList: List[ArcNode]
}

case class Leaf(value: Arc, parent: BSTree) extends BSTree {
  def toList = Nil
}

case class Node(left: BSTree, value: SiteTuple, right: BSTree, parent: BSTree) extends BSTree {
  def toList = left.toList ::: value :: right.toList
}

object Tree {

  def insertLeaf(x: Arc, tree: BSTree)

  def insert(x: ArcNode, tree: BSTree) = x match {
    case v : Arc => insertLeaf(v, tree)
    case v : SiteTuple => insertNode(v, tree)
  }
  def insertNode(x: SiteTuple, tree: BSTree)(implicit o : Ordering[ArcNode]): BSTree = tree match {
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