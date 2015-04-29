import com.vividsolutions.jts.geom.{Polygon, Coordinate, Point}
import structure.{IDCEL, DCEL}
import scala.collection.mutable
/**
 * Created by ludov on 27/04/15.
 */

trait Event {
  def y : Double
}

case class CircleEvent(a: Arc, y: Double) extends Event
case class SiteEvent(site: Coordinate, y: Double) extends Event

class Fortune {

  var q = new mutable.PriorityQueue[Event]()(Ordering.by(_.y))
  val edgeList = DCEL
  import edgeList._
  var tree : BSTree = EmptyT()

  def run(points: Array[Coordinate]): Unit ={
    for(i <- 0 until points.length){
      q.enqueue(new SiteEvent(points(i), points(i).y))
    }
    while(!q.isEmpty){
      val event = q.dequeue()
      event match {
        case e : SiteEvent => handleSiteEvent(e.site)
        case e : CircleEvent => handleCircleEvent(Tree.search(e.a,tree), e.y)
      }
    }
  }

  def getPolygons : List[Polygon] = {
    edgeList.faces
    ???
  }

  def handleCircleEvent(l : Leaf, sweepY: Double) = {
    val a = l.value
    val center = computeCenter(a)
    center match {
      case Some(c) => {
        //Handle half edges
        val rightEdge = {
          if (l.parent.left == l) {
            l.parent.value.edge
          } else {
            val tmp = Tree.findRight(l.parent).value.edge
            if(tmp.origin == null) tmp
            else tmp.twin
          }
        }
        val leftEdge = {
          if (l.parent.right == l) {
            l.parent.value.edge
          } else {
            val tmp = Tree.findRight(l.parent).value.edge
            if(tmp.origin == null) tmp
            else tmp.twin
          }
        }
        val centerEdge = HalfEdge(null, null, null, null, null)
        val newEdge = HalfEdge(null, null, null, null, null)
        val vertex = Vertex(c, newEdge)

        newEdge.origin = vertex
        centerEdge.twin = newEdge
        newEdge.twin = centerEdge

        rightEdge.twin.origin = vertex
        leftEdge.twin.origin = vertex

        rightEdge.next = leftEdge.twin
        leftEdge.next = centerEdge.twin
        centerEdge.next = rightEdge.twin

        leftEdge.twin.prev = rightEdge
        centerEdge.twin.prev = leftEdge
        rightEdge.twin.prev = centerEdge


        //Handle suppression in the tree
        a match {
          case Arc(site, Some(pred), Some(next), event) =>
            Tree.removeArcNode(l, newEdge)

            q = q.filter {
              case CircleEvent(b) => b.site == pred.site || (b.site == next.site)
              case _ => true
            }


        }

        a.pred.foreach(checkCircleEvent(_, sweepY)) //Check the triple of consecutive arcs where the a is the right arc
        a.next.foreach(checkCircleEvent(_, sweepY)) //Check the triple of consecutive arcs where the a is the left arc

      }
    }
  }

  def handleSiteEvent(p: Coordinate) = {
    val newArc = new Arc(p, None, None, None)
    if (tree.isEmpty) {
      tree = Leaf(newArc, null)
    }
    else {

      //Create Half-Edges
      val (h1, h2) = edgeList.createEdge

      val old = Tree.addParabola(newArc, h1, tree) // create and add the subtree, link the half-edge with internal nodes, link newArc with pred/next
      old.value.event.foreach(toRemove => q = q.filterNot(event => toRemove == event)) // remove false alarm

      newArc.pred.foreach(checkCircleEvent(_, p.y)) //Check the triple of consecutive arcs where the new arc is the right arc
      newArc.next.foreach(checkCircleEvent(_, p.y)) //Check the triple of consecutive arcs where the new arc is the left arc
    }
  }

  def checkCircleEvent(a : Arc, sweepY : Double) = {
    //check if there is a triple
    //if computeCenter return None, it means that there is no triple
    computeCenter(a).foreach { center =>
      center
      val p = a.site
      val r = Math.sqrt(Math.pow(center.x - p.x, 2) + Math.pow(center.y - p.y, 2))

      if (center.y - r <= sweepY) {
        //add event
        val event = CircleEvent(a, center.y - r)
        a.event = Some(event)
      }
    }
  }

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
