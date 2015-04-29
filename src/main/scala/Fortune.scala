
import com.vividsolutions.jts.geom._

import structure.DCEL
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
  val factory = new GeometryFactory()

  def run(points: Array[Coordinate]): Unit ={
    for(i <- 0 until points.length){
      q.enqueue(new SiteEvent(points(i), points(i).y))
    }
    while(!q.isEmpty){
      val event = q.dequeue()
      event match {
        case e : SiteEvent => handleSiteEvent(e.site)
        case e : CircleEvent => handleCircleEvent(Tree.search(e.a,tree)(new NodeOrdering(e.y)), e.y)
      }
    }
    val multipoint = factory.createMultiPoint(points)
    val pointsV = edgeList.vertices.map(v => new Coordinate(v.point.x, v.point.y))
    val multipointV = factory.createMultiPoint(pointsV.toArray)
    val allPoints = multipointV.union(multipoint)
    val env = allPoints.getEnvelopeInternal
    val treeL = tree.toList
    val ord = new NodeOrdering(env.getMinY-10)
    treeL.foreach {
      case p : SiteTuple => {
        //val (p1, p2) = p.sites
        //val a = (p1.x - p2.x) / (p2.y - p1.y)
        //val b = (Math.pow(p2.y, 2) + Math.pow(p2.x, 2) - Math.pow(p1.x, 2) - Math.pow(p1.y, 2)) / (2 * (p2.y - p1.y))

        p.edge match {
          case HalfEdge(origin, _, _, _, _) if origin == null => {
            val orig = new Vertex(ord.breakPoint(p.sites), p.edge)
            p.edge.origin = orig
          }
          case _ => {
            val orig = new Vertex(ord.breakPoint(p.sites), p.edge.twin)
            p.edge.twin.origin = orig
          }
        }
      }
    }

    createLinesFromEdges
  }

  def getPolygons : List[Polygon] = {
    val polygons = edgeList.faces.map { face =>
      val start = face.edge.origin
      var edge = face.edge.next
      val points = ArrayBuffer[Coordinate](start.point)
      while (start != edge.origin) {
        points += edge.origin.point
        edge = edge.next
      }

      factory.createPolygon(points.toArray)
    }
    polygons.toList

  }

  def createLinesFromEdges : MultiLineString = {
    val lines = edgeList.edges.map { edge =>
      val p1 = edge.origin.point
      val p2 = edge.origin.point

      factory.createLineString(Array(p1, p2))
    }.toArray

    factory.createMultiLineString(lines)
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

        val (centerEdge, newEdge) = edgeList.createEdge
        val vertex = Vertex(c, newEdge)
        edgeList.vertices.add(vertex)

        newEdge.origin = vertex

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
              case CircleEvent(b, _) => b.site == pred.site || (b.site == next.site)
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

      val old = Tree.addParabola(newArc, h1, tree)(new NodeOrdering(p.y)) // create and add the subtree, link the half-edge with internal nodes, link newArc with pred/next
      old.value.event.foreach(toRemove => q = q.filterNot(event => toRemove == event)) // remove false alarm

      newArc.pred.foreach(checkCircleEvent(_, p.y)) //Check the triple of consecutive arcs where the new arc is the right arc
      newArc.next.foreach(checkCircleEvent(_, p.y)) //Check the triple of consecutive arcs where the new arc is the left arc
    }
  }

  def checkCircleEvent(a : Arc, sweepY : Double) = {
    //check if there is a triple
    //if computeCenter return None, it means that there is no triple
    computeCenter(a).foreach { center =>
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
