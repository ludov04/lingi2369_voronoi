

import _root_.util.Util
import com.vividsolutions.jts.geom._

import structure._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
 * AUTHORS :
 * Fabian Souris
 * Ludovic Vanoorenberghe
 */

/**
 * A class that implement Fortune's algorithm to compute a Voronoi Diagram
 * @param points The set of points (sites) for which the Voronoi Diagram is computed
 */
class Fortune(val points: Array[Coordinate], winWidth: Int, winHeight: Int) extends Voronoi {

  import Fortune._

  /**
   * Priority Queue containing the sites and circle event
   * Higher y have higher priority
   */
  var q = new mutable.PriorityQueue[Event]()(Ordering.by(_.y))
  /**
   * Doubly connected edge list structure to store the internal representation of the Voronoi
   * http://en.wikipedia.org/wiki/Doubly_connected_edge_list
   */
  val edgeList = new DCEL()

  /**
   * A Binary Tree structure to store the internal representation of the beachline
   */
  var tree : BSTree = EmptyT()

  /**
   * Enqueue the site event on initialisation
   */
  for(i <- 0 until points.length){
    q.enqueue(new SiteEvent(points(i), points(i).y))
  }

  /**
   * The y coordinate of the sweepline on the last iteration
   */
  var lastY = Double.PositiveInfinity

  /**
   *  Run a single iteration of the algorithm, useful for debugging or to show the progess on some UI
   * @return true if the queue is Empty i.e. if there is no more iteration to run
   *         false if there is still some events of the queue i.e. the algorithm is not finished
   */
  def runStep() : Boolean = {
    if(q.nonEmpty){
      val event = q.dequeue()
      event match {
        case e : SiteEvent => handleSiteEvent(e.site)
        case e : CircleEvent => handleCircleEvent(Tree.search(e.a,tree)(new NodeOrdering(e.y)), e.y)
      }
      lastY = event.y
      q.isEmpty
    } else true

  }

  /**
   * Run all the iteration until the queue is empty and compute the diagram from the doubly-connected edge list and a bounding box
   * @return
   */
  def run = {
    while(!runStep()) { }
    computeDiagram()
  }


  /**
   * Handle a circle event
   * see Computational geometry De Berg, Mark de Berg, Otfried Cheong, Marc van Kreveld, Mark Overmars, 157-158, 2008
   * @param l the leaf of tree representing the arc that will disappear
   * @param sweepY the y coordinate of the sweepline
   */
  def handleCircleEvent(l : Leaf, sweepY: Double) = {
    val a = l.value
    val center = computeCenter(a)
    center.foreach { c =>

        //Handle half edges
        val rightEdge = Tree.findRight(l).value.edge
        val leftEdge = Tree.findLeft(l).value.edge

        val (centerEdge, newEdge) = edgeList.createEdge((a.pred.get.site, a.next.get.site))
        val vertex = Vertex(c, centerEdge)
        edgeList.vertices += vertex

        centerEdge.origin = vertex

        rightEdge.origin = vertex
        leftEdge.origin = vertex

        rightEdge.twin.next = centerEdge
        leftEdge.twin.next = rightEdge
        centerEdge.twin.next = leftEdge

        centerEdge.prev = rightEdge.twin
        rightEdge.prev = leftEdge.twin
        leftEdge.prev = centerEdge.twin


        //Handle suppression in the tree
        a match {
          case Arc(site, Some(pred), Some(next), event) =>
            q = q.filter {
              case CircleEvent(b, _) => ((b.site != pred.site || b.next.get.site != site)  && (b.site != next.site || b.pred.get.site != site)) && b != a
              case _ => true
            }
            tree = Tree.removeArcNode(l, newEdge, tree)
        }

        a.pred.foreach(checkCircleEvent(_, sweepY)) //Check the triple of consecutive arcs where the a is the right arc
        a.next.foreach(checkCircleEvent(_, sweepY)) //Check the triple of consecutive arcs where the a is the left arc

    }
  }

  /**
   * Handle a site event
   * see Computational geometry De Berg, Mark de Berg, Otfried Cheong, Marc van Kreveld, Mark Overmars, 157-158, 2008
   * @param p the site at which the event is occuring
   */
  def handleSiteEvent(p: Coordinate) = {
    val newArc = new Arc(p, None, None, None)
    if (tree.isEmpty) {
      tree = Leaf(newArc, null)
    }
    else {

      val (old, newTree) = Tree.addParabola(newArc, tree, edgeList)(new NodeOrdering(p.y)) // create and add the subtree, link the half-edge with internal nodes, link newArc with pred/next
      tree = newTree
      old.value.event.foreach(toRemove => q = q.filterNot(event => toRemove == event)) // remove false alarm

      newArc.pred.foreach(checkCircleEvent(_, p.y)) //Check the triple of consecutive arcs where the new arc is the right arc
      newArc.next.foreach(checkCircleEvent(_, p.y)) //Check the triple of consecutive arcs where the new arc is the left arc
    }
  }

  /**
   * Check if an arc is going to disappear in the future due to a circle event
   * @param a the arc to check
   * @param sweepY the current y coordinate of the sweepline
   */
  def checkCircleEvent(a : Arc, sweepY : Double) = {
    //check if there is a triple
    //if computeCenter return None, it means that there is no triple
    computeCenter(a).foreach { center =>
      val p = a.site
      val r = Math.sqrt(Math.pow(center.x - p.x, 2) + Math.pow(center.y - p.y, 2))

      if (center.y - r <= sweepY) {
        val ord = new NodeOrdering(center.y - r)
        val b1 = ord.breakPoint((a.pred.get.site, a.site))
        val b2 = ord.breakPoint((a.site, a.next.get.site))
        if(Util.round(b1.x) == Util.round(b2.x)) {
          //add event
          val event = CircleEvent(a, center.y - r)

          a.event = Some(event)
          q += event
        }
      }
    }
  }

  /**
   * Create the segment from an edge list. This assumes that the half-edge in the list
   * are connected to their twin and their origin
   * @param edges a list of HalfEdge
   * @return A GeometryCollection containing all the segment coresponding to edges
   */
  def createLinesFromEdges(edges: List[HalfEdge]) : MultiLineString = {
    val lines = edges.filter { edge =>
      edge.origin != null && edge.twin.origin != null
    }.map { edge =>
      val p1 = edge.origin.point
      val p2 = edge.twin.origin.point

      factory.createLineString(Array(p1, p2))
    }.toArray

    factory.createMultiLineString(lines)
  }

  /**
   * Connect the remaining dangling half-edges to the bounding box to have a true subdivision of the plane
   * @param box the bounding box
   * @param y the y coordinate of the sweepline
   */
  def connectToBox(box: Envelope, y: Double) = {

    def choose(tuple: (Coordinate, Coordinate), x1: Coordinate, x2: Coordinate) = {
      val breakpoint = new NodeOrdering(y).breakPoint(tuple)
      val x1Distance = Util.distance(x1, breakpoint)
      val x2Distance = Util.distance(x2, breakpoint)

      if (x1Distance <= x2Distance) x1
      else x2
    }

    tree.toList.foreach {
      case node : SiteTuple =>
        val sites = node.sites
        val (x1, x2) = computeBisector(sites._1, sites._2, box)
        val inter = choose(sites, x1, x2)
        if(node.edge.origin == null) {
          val orig = new Vertex(inter,node.edge)
          node.edge.origin = orig
        }
    }
  }

  /**
   * Compute a bounding box, connect the half-edge to it, and compute a GeometryCollection representing the Voronoi Diagram
   * @return a GeometryCollection representing the Voronoi Diagram
   */
  def computeDiagram() : GeometryCollection = {
    val multipoint = factory.createMultiPoint(points)
    val pointsV = edgeList.vertices.map(v => new Coordinate(v.point.x, v.point.y))
    val multipointV = factory.createMultiPoint(pointsV.toArray)
    val allPoints = multipointV.union(multipoint)

    val env = allPoints.getEnvelopeInternal
    val expandBy: Double = Math.max(env.getWidth, env.getHeight)
    env.expandBy(expandBy)


    //connectToBox(env, lastY)
    lastY -= 10
    computeStepDiagram()
    //createLinesFromEdges(edgeList.edges.toList)
  }

  def computeStepDiagram() : GeometryCollection = {
    val ord = new NodeOrdering(lastY)
    def bpEdge(edge: HalfEdge) : Coordinate = {
      if(edge.origin == null){
        if(edge.twin.origin == null) ord.breakPoint(edge.sites)
        else {
          var currArc = tree.getLeftMost.value
          while (currArc.next.isDefined) {
            if ((currArc.site, currArc.next.get.site) == edge.sites) {
              return ord.breakPoint(edge.sites)
            } else if ((currArc.next.get.site, currArc.site) == edge.sites) {
              return ord.breakPoint(edge.twin.sites)
            }
            currArc = currArc.next.get
          }
          println(edge.sites._1 + " -- " + edge.sites._2)
          ord.breakPoint(edge.sites)
        }
      }
      else edge.origin.point
    }

    val lines = edgeList.edges.map { edge =>
      val p1 = bpEdge(edge)
      val p2 = bpEdge(edge.twin)
      factory.createLineString(Array(p1, p2))
    }.toArray

    factory.createMultiLineString(lines)
  }

  /**
   *
   * @return
   */
  def getBeachLine : MultiLineString = {
    val beachline = ArrayBuffer[LineString]()
    var currArc = Option(tree.getLeftMost.value)
    while(currArc.isDefined){
      val parabola = getParabola(currArc.get, lastY)
      if(parabola.length >= 2) beachline += factory.createLineString(parabola)
      currArc = currArc.get.next
    }
    beachline += factory.createLineString(Array(new Coordinate(0, lastY), new Coordinate(winWidth, lastY)))
    factory.createMultiLineString(beachline.toArray)
  }

  /**
   *
   * @param currArc
   * @param yd
   * @return
   */
  def getParabola(currArc: Arc, yd: Double): Array[Coordinate] = {
    val ord = new NodeOrdering(yd)
    val b1 = {
      if (currArc.pred.isEmpty) {
        Double.MinValue
      } else {
        ord.breakPoint((currArc.pred.get.site, currArc.site)).x
      }
    }
    val b2 = {
      if (currArc.next.isEmpty) {
        Double.MaxValue
      } else {
        ord.breakPoint((currArc.site, currArc.next.get.site)).x
      }
    }
    val parabola = ArrayBuffer[Coordinate]()
    val p = currArc.site.y - yd
    if(p == 0){
      parabola ++= Array(currArc.site, new Coordinate(currArc.site.x, yd))
    } else {
      for (x <- 0 until winWidth) {
        if(x >= b1 && x <= b2) {
          val y = Math.pow(x - currArc.site.x, 2) / (2 * p) + currArc.site.y - (p / 2)
          parabola += new Coordinate(x, y)
        }
      }
    }
    parabola.toArray
  }

}

object Fortune {
  def computeCenter(a: Arc) : Option[Coordinate] = {
    a match {
      case Arc(site, Some(pred), Some(next), event) =>
        if (next.site == pred.site) {
          return None
        }
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
      case _ => None
    }
  }


}
