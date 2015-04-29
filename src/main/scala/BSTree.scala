import com.vividsolutions.jts.geom.Coordinate

import structure.DCEL._

/**
 * Created by Fabian on 27-04-15.
 */


sealed trait ArcNode

case class Arc(site: Coordinate, var pred: Option[Arc], var next: Option[Arc], var event: Option[CircleEvent]) extends ArcNode

case class SiteTuple(var sites: (Coordinate, Coordinate), var edge: HalfEdge) extends ArcNode


class NodeOrdering(y : Double) extends Ordering[ArcNode] {

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
      case SiteTuple(aSites, _) => b match {
          case SiteTuple(bSites, _) => aSites.x.compareTo(bSites.x)
          case Arc(site, _, _, _) => aSites.x.compareTo(site.x)
        }
      case Arc(site, _, _, _) => b match {
          case SiteTuple(bSites, _) => site.x.compareTo(bSites.x)
          case Arc(p, _, _, _) => site.x.compareTo(p.x)
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
  def isEmpty : Boolean
}

case class EmptyT() extends BSTree {
  def value = null
  def parent = null
  def toList = Nil
  def getLeftMost = null
  def getRightMost = null
  def isEmpty = true
}

case class Leaf(value: Arc, var parent: Node) extends BSTree {
  def toList = Nil
  def getLeftMost = this
  def getRightMost = this
  def isEmpty = false
}

case class Node(var left: BSTree, value: SiteTuple, var right: BSTree, var parent: Node) extends BSTree {
  def toList = left.toList ::: value :: right.toList

  def getLeftMost = left.getLeftMost

  def getRightMost = right.getRightMost

  def isEmpty = false
}

object Tree {

  def removeArcNode(x: Leaf, edge: HalfEdge): BSTree ={
    x match {
      case Leaf(value, parent) if parent == null => {
        val emptyT = new EmptyT()
        emptyT
      }
      case Leaf(valueL, parentL) => {
        val predTmp = valueL.pred
        val nextTmp = valueL.next
        if(predTmp.isDefined) valueL.pred = nextTmp
        if(nextTmp.isDefined) valueL.next = predTmp

        parentL match {
          case Node(leftN, valueN, rightN, parentN) if leftN == x => {
            if(parentN == null) rightN
            else {
              val leftBound = findLeft(parentN)
              if(leftBound != null) {
                leftBound.value.sites = (leftBound.value.sites._1, rightN.getLeftMost.value.site)
                leftBound.value.edge = edge
              }
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
              val rightBound = findRight(parentN)
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

  /**
   *
   * @param a
   * @param tree
   * @return the leaf that were replaced
   */
  def addParabola(a : Arc, h: HalfEdge, tree: BSTree)(implicit o : NodeOrdering) : Leaf = {
    val node = search(a.site, tree)

    val leftArc = node.value.copy(next = Some(a))
    val rightArc = node.value.copy(pred = Some(a))
    //update links
    a.pred = Some(leftArc)
    a.next = Some(rightArc)

    val leaftLeaf = Leaf(leftArc, null)
    val newLeaf = Leaf(a, null)
    val rightLeaf = Leaf(rightArc, null)

    val sub = Node(leaftLeaf, SiteTuple((leftArc.site, a.site), h), newLeaf, null )
    leaftLeaf.parent = sub
    newLeaf.parent = sub

    val newTree = Node(sub, SiteTuple((a.site, rightArc.site), h), rightLeaf, node.parent)
    sub.parent = newTree
    rightLeaf.parent = newTree

    replaceNode(node, newTree, tree)
    node
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
    if(oldNode.parent == null) return newNode
    else if(oldNode.parent.left == oldNode) oldNode.parent.left = newNode
    else if(oldNode.parent.right == oldNode) oldNode.parent.right = newNode

    tree

  }


  def search(x: Coordinate, tree: BSTree)(implicit o : NodeOrdering): Leaf = {
    import o._
    tree match {
      case v: Leaf => v
      case Node(left, value, right, parent) if x.x < value.sites.x => search(x, left)
      case Node(left, value, right, parent) if x.x >= value.sites.x => search(x, right)
      case _: EmptyT => throw new UnsupportedOperationException
    }
  }

  def search(a: Arc, tree: BSTree)(implicit o : NodeOrdering): Leaf = {
    import o._
    a match {
      case Arc(_, None, _, _) => tree.getLeftMost
      case Arc(_, _, None, _) => tree.getRightMost
      case Arc(valA, Some(pred), Some(next), _) => {
        tree match {
          case v: Leaf => v
          case Node(left, value, right, parent) if breakPoint((a.site, next.site)).x < value.sites.x => search(a, left)
          case Node(left, value, right, parent) if breakPoint((pred.site, a.site)).x > value.sites.x => search(a, right)
        }
      }
    }
  }

}