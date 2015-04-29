import com.vividsolutions.jts.geom.Coordinate

/**
 * Created by Fabian on 27-04-15.
 */


sealed trait ArcNode

case class Arc(site: Coordinate, var pred: Option[Arc], var next: Option[Arc], var event: Option[CircleEvent]) extends ArcNode
case class SiteTuple(var sites: (Coordinate, Coordinate)) extends ArcNode

class NodeOrdening(y : Int) extends Ordering[ArcNode] {

  implicit def breakPoint(sites : (Coordinate, Coordinate)): Coordinate = {
    val a = (1/(sites._1.y-y))-(1/(sites._2.y-y))
    val b = (-2*sites._1.x/(sites._1.y-y))+(2*sites._2.x/(sites._2.y-y))
    val c = sites._1.y-sites._2.y
    val delta = Math.pow(b, 2) - (4*a*c)
    val s1 = (-b-Math.sqrt(delta))/(2*a)
    val s2 = (-b+Math.sqrt(delta))/(2*a)
    if(sites._1.y > sites._2.y){
      val sy = Math.pow(s1-sites._1.x, 2)/(2*(sites._1.y-y))+((sites._1.y+sites._2.y)/2)
      new Coordinate(s1, sy)
    } else {
      val sy = Math.pow(s2-sites._1.x, 2)/(2*(sites._1.y-y))+((sites._1.y+sites._2.y)/2)
      new Coordinate(s2, sy)
    }
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
  def value : ArcNode
  def parent : Node
  def toList: List[ArcNode]
  def getLeftMost : Leaf
  def getRightMost : Leaf
}

case class EmptyT() extends BSTree {
  def value = null
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
              val leftBound = findLeft(parentN)
              if(leftBound != null) leftBound.value.sites = (leftBound.value.sites._1, rightN.getLeftMost.value.site)
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
              val rightBound = findLeft(parentN)
              if(rightBound != null) rightBound.value.sites = (rightBound.value.sites._1, leftN.getRightMost.value.site)
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

  def findLeft(x: BSTree): Node = {
    val parent = x.parent
    if (parent != null) {
      if(parent.left == x) {
        findLeft(parent)
      }
      else if (parent.right == x) {
        parent
      }
      else null
    } else {
      null
    }

  }

  def findRight(x: BSTree): Node = {
    val parent = x.parent
    if (parent != null) {
      if(parent.left == x) {
        parent
      }
      else if (parent.right == x) {
        findRight(parent)
      }
      else null
    } else {
      null
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
    val rightParent = findRight(oldNode)
    val rightIntersection = rightParent.value.sites
    val leftParent = findLeft(oldNode)
    val leftIntersection = leftParent.value.sites
    rightParent.value.sites = ( right.value.site , rightIntersection._2)
    leftParent.value.sites = (leftIntersection._1, left.value.site)

    //replace the node
    oldNode match {
      case oldNode.parent.left => oldNode.parent.left = newNode
      case oldNode.parent.right => oldNode.parent.right = newNode
    }

    tree

  }


  def search(x: Coordinate, tree: BSTree)(implicit o : Ordering[ArcNode]): Leaf = {
    import o._
    tree match {
      case Leaf => false
      case Node(left, value, right) if x < value => search(x, left)
      case Node(left, value, right) if x > value => search(x, right)
      case _ => true
    }
  }

}