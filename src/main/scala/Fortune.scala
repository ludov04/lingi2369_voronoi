import com.vividsolutions.jts.geom.{Coordinate, Point}
import structure.DCEL
import structure.DCEL._

import scala.collection.mutable
/**
 * Created by ludov on 27/04/15.
 */

trait Event

case class CircleEvent(a: Arc) extends Event
case class SiteEvent() extends Event

class Fortune {

  var q = new mutable.PriorityQueue[Event]()
  val edgeList : DCEL = new DCEL()
  var tree = EmptyT()

  def handleCircleEvent(l : Leaf) = {
    val a = l.value
    val center = computeCenter(a)
    center match {
      case Some(c) => {
        //Handle half edges
        val rightEdge = {
          if (l.parent.left == l) {
            l.parent.value.edge
          } else {
            Tree.findRight(l.parent).value.edge
          }
        }
        val leftEdge = {
          if (l.parent.right == l) {
            l.parent.value.edge
          } else {
            Tree.findRight(l.parent).value.edge
          }
        }
        val newEdgeTwin = HalfEdge(null, null, null, null, null)
        val newEdge = HalfEdge(null, null, null, null, null)
        val vertex = Vertex(c, newEdge)
        newEdge.origin = vertex
        

        //Handle suppression in the tree
        a match {
          case Arc(site, Some(pred), Some(next), event) =>
            Tree.removeArcNode(l, newEdge)

            q = q.filter {
              case CircleEvent(b) => b.site == pred.site || (b.site == next.site)
              case _ => true
            }


        }

      }
    }
  }

  def handleSiteEvent(p: Coordinate) = {
    val newArc = new Arc(p, None, None, None)
    if (tree.isEmpty) {
      Tree.insert(newArc, tree)
    }
    else {
      val old = Tree.addParabola(newArc, tree) // the leaf containing the arc vertically above p
      old.value.event.foreach(toRemove => q = q.filterNot(event => toRemove == event)) // remove false alarm
    }
  }

  def checkCircleEvent(a : Arc) = ???

  def computeCenter(a: Arc) : Option[Coordinate] = {
    a match {
      case Arc(site, Some(pred), Some(next), event) => {
        var p1 = pred.site
        var p2 = site
        var p3 = next.site
        if(pred.site.y == site.y) {
          p1 = site
          p2 = next.site
          p3 = pred.site
        } else if(site.y == next.site.y){
          p1 = next.site
          p2 = pred.site
          p3 = site
        }

        val a1 = (p1.x - p2.x) / (p2.y - p1.y)
        val b1 = (Math.pow(p2.y, 2) + Math.pow(p2.x, 2) - Math.pow(p1.x, 2) - Math.pow(p1.y, 2)) / (2 * (p2.y - p1.y))
        val a2 = (p2.x - p3.x) / (p3.y - p2.y)
        val b2 = (Math.pow(p3.y, 2) + Math.pow(p3.x, 2) - Math.pow(p2.x, 2) - Math.pow(p2.y, 2)) / (2 * (p3.y - p2.y))

        val sx = (b2-b1)/(a1-a2)
        val sy = (a1*sx)+b1

        Some(new Coordinate(sx, sy))
      }
      case _ => None
    }
  }

}
