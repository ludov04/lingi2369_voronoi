import com.vividsolutions.jts.geom.Coordinate

import structure.DCEL._

/**
 * Created by Fabian on 27-04-15.
 */


sealed trait ArcNode

case class Arc(site: Coordinate, var pred: Option[Arc], var next: Option[Arc], var event: Option[CircleEvent]) extends ArcNode {
  override def toString = site.toString
}

case class SiteTuple(var sites: (Coordinate, Coordinate), var edge: HalfEdge) extends ArcNode {
  override def toString = sites.toString
}


class NodeOrdering(y : Double) extends Ordering[ArcNode] {

  implicit def breakPoint(sites : (Coordinate, Coordinate)): Coordinate = {
    val a = (1/(sites._1.y-y))-(1/(sites._2.y-y))
    val b = (-2*sites._1.x/(sites._1.y-y))+(2*sites._2.x/(sites._2.y-y))
    val c = (Math.pow(sites._1.x,2)/(sites._1.y-y))-(Math.pow(sites._2.x,2)/(sites._2.y-y))+sites._1.y-sites._2.y
    val delta = Math.pow(b, 2) - (4*a*c)
    val s1 = (-b-Math.sqrt(delta))/(2*a)
    val s2 = (-b+Math.sqrt(delta))/(2*a)
    //println(s1.toString() + " -- " + s2.toString())
    if(sites._1.y > sites._2.y){
      val sy = Math.pow(Math.min(s1, s2)-sites._1.x, 2)/(2*(sites._1.y-y))+y
      new Coordinate(Math.min(s1, s2), sy)
    } else {
      val sy = Math.pow(Math.max(s1, s2)-sites._1.x, 2)/(2*(sites._1.y-y))+y
      new Coordinate(Math.max(s1, s2), sy)
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

object testrun {
  def main(args: Array[String]): Unit = {
    val ord = new NodeOrdering(0)
    println(ord.breakPoint((new Coordinate(0,10), new Coordinate(10,1))))
  }
}

sealed trait BSTree {
  def value : ArcNode
  var parent : Node
  def toList: List[ArcNode]
  def getLeftMost : Leaf
  def getRightMost : Leaf
  def isEmpty : Boolean
}

case class EmptyT(var parent: Node = null) extends BSTree {
  def value = null
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
// class Tree(var root : BSTree) {
object Tree {

  def removeArcNode(x: Leaf, edge: HalfEdge, root: BSTree): BSTree ={
    x match {
      case Leaf(value, parent) if parent == null => {
        val emptyT = new EmptyT()
        // root = emptyT
        emptyT
      }
      case Leaf(valueL, parentL) => {
        val predTmp = valueL.pred
        val nextTmp = valueL.next
        if(predTmp.isDefined) valueL.pred = nextTmp
        if(nextTmp.isDefined) valueL.next = predTmp

        parentL match {
          case Node(leftN, valueN, rightN, parentN) if leftN == x => {
            if(parentN == null) {
              rightN.parent = null
              rightN
            }
            else {
              val leftBound = findLeft(parentN)
              if(leftBound != null) {
                leftBound.value.sites = (leftBound.value.sites._1, rightN.getLeftMost.value.site)
                leftBound.value.edge = edge
              }
              parentN match {
                case Node(leftPN, valuePN, rightPN, parentPN) if leftPN == parentL => {
                  parentN.left = rightN
                  rightN.parent = parentN
                  root
                }
                case Node(leftPN, valuePN, rightPN, parentPN) => {
                  parentN.right = rightN
                  rightN.parent = parentN
                  root

                }
              }
            }
          }
          case Node(leftN, valueN, rightN, parentN) => {
            if(parentN == null) {
              leftN.parent = null
              leftN
            }
            else {
              val rightBound = findRight(parentN)
              if(rightBound != null) {
                rightBound.value.sites = (rightBound.value.sites._1, leftN.getRightMost.value.site)
                rightBound.value.edge = edge
              }
              parentN match {
                case Node(leftPN, valuePN, rightPN, parentPN) if leftPN == parentL => {
                  parentN.left = leftN
                  leftN.parent = parentN
                  root
                }
                case Node(leftPN, valuePN, rightPN, parentPN) => {
                  parentN.right = leftN
                  leftN.parent = parentN
                  root

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
  def addParabola(a : Arc, tree: BSTree)(implicit o : NodeOrdering) : (Leaf, BSTree) = {
    val node = search(a.site, tree)

    val leftArc = node.value.copy(next = Some(a))
    val rightArc = node.value.copy(pred = Some(a))
    //update links
    a.pred = Some(leftArc)
    a.next = Some(rightArc)

    val leftLeaf = Leaf(leftArc, null)
    val newLeaf = Leaf(a, null)
    val rightLeaf = Leaf(rightArc, null)

    val (h,_) = createEdge((a.pred.get.site, a.site))

    val sub = Node(leftLeaf, SiteTuple((leftArc.site, a.site), h), newLeaf, null )
    leftLeaf.parent = sub
    newLeaf.parent = sub

    val newTree = Node(sub, SiteTuple((a.site, rightArc.site), h), rightLeaf, node.parent)
    sub.parent = newTree
    rightLeaf.parent = newTree

    (node, replaceNode(node, newTree, tree))
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


    if(oldNode.parent == null) return newNode

    //Update the tuples
    val rightParent = findRight(oldNode)
    val leftParent = findLeft(oldNode)

    if (rightParent != null) {
      val rightIntersection = rightParent.value.sites
      rightParent.value.sites = ( right.value.site , rightIntersection._2)
    }
    if (leftParent != null) {
      val leftIntersection = leftParent.value.sites
      leftParent.value.sites = (leftIntersection._1, left.value.site)
    }

    //replace the node
    if(oldNode.parent.left == oldNode) oldNode.parent.left = newNode
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
      case Arc(_, None, _, _) => {
        println("fin a wrong arc : leftMost arc")
        tree.getLeftMost
      }
      case Arc(_, _, None, _) => {
        println("fin a wrong arc : rightMost arc")
        tree.getRightMost
      }
      case Arc(valA, Some(pred), Some(next), _) => {
        tree match {
          case v: Leaf => {
            if(v.value != a) println("find a wrong arc : other")
            v
          }
          case Node(left, value, right, parent) if round((breakPoint((a.site, next.site)).x+breakPoint((pred.site, a.site)).x)/2) < round(value.sites.x) => search(a, left)(o)
          case Node(left, value, right, parent) if round((breakPoint((a.site, next.site)).x+breakPoint((pred.site, a.site)).x)/2) == round(value.sites.x) => {
            println("equal")
            if(value.sites._2 == a.site) search(a, right)(o)
            else search(a, left)(o)
          }
          case Node(left, value, right, parent) => search(a, right)(o)
        }
      }
    }
  }

  def round(x: Double) = {
    Math.floor(x * 100) / 100
  }

}