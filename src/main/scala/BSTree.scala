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
}

case class Leaf(value: Arc, var parent: BSTree) extends BSTree {
  def toList = Nil
}

case class Node(left: BSTree, value: SiteTuple, right: BSTree, var parent: BSTree) extends BSTree {
  def toList = left.toList ::: value :: right.toList
}

object Tree {

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
        val node = Node(tree, SiteTuple((x.site, value.site)), newLeaf, parent)
        newLeaf.parent = node
        node
      }
    }
  }

  def insert(x: ArcNode, tree: BSTree)(implicit o : Ordering[ArcNode]) : BSTree = {
    import o._
    tree match {
      case t : Leaf => x match {
        case v : Arc => insertLeaf(v, t)
        case v : SiteTuple => insertNode(v, t)
      }
      case Node(left, value, right, parent) if x < value => {
        Node(insert(x, left), value, right, parent)
      }
      case Node(left, value, right, parent) => {
        Node(left, value, insert(x, right), parent)
      }
    }
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

  def search(x: ArcNode, tree: BSTree)(implicit o : Ordering[ArcNode]): Boolean = {
    import o._
    tree match {
      case Leaf => false
      case Node(left, value, right) if x < value => search(x, left)
      case Node(left, value, right) if x > value => search(x, right)
      case _ => true
    }
  }

  def tsort[T <% Ordered[T]](values: List[T]) = values.foldRight(Leaf: BSTree[T])(insert).toList
}