
import com.vividsolutions.jts.geom._

import structure._
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
  val edgeList = new DCEL()
  import edgeList._
  var tree : BSTree = EmptyT()
  val factory = new GeometryFactory()

  def runStep(points: Array[Coordinate], nStep: Int) : (Int, MultiLineString) = {
    if(nStep == 0) {
      for(i <- 0 until points.length){
        q.enqueue(new SiteEvent(points(i), points(i).y))
      }
    }
    if(!q.isEmpty){
      val event = q.dequeue()
      event match {
        case e : SiteEvent => handleSiteEvent(e.site)
        case e : CircleEvent => {
          //Tree.printTree(tree)
          //println(e.a.pred.get.site + " -- " + e.a.site + " -- " + e.a.next.get.site)
          handleCircleEvent(Tree.search(e.a,tree)(new NodeOrdering(e.y)), e.y)
        }
      }
      val beachline = ArrayBuffer[LineString]()
      var currArc = Option(tree.getLeftMost.value)
      while(currArc.isDefined){
        beachline += getParabola(currArc.get, event.y)
        currArc = currArc.get.next
      }
      beachline += factory.createLineString(Array(new Coordinate(0, event.y), new Coordinate(1000, event.y)))
      (q.size, factory.createMultiLineString(beachline.toArray))
    } else {
      println("ok")
      val multipoint = factory.createMultiPoint(points)
      val pointsV = edgeList.vertices.map(v => new Coordinate(v.point.x, v.point.y))
      val multipointV = factory.createMultiPoint(pointsV.toArray)
      val allPoints = multipointV.union(multipoint)
      val env = allPoints.getEnvelopeInternal
      val expandBy: Double = Math.max(env.getWidth, env.getHeight)
      env.expandBy(expandBy)
      val treeL = tree.toList


      connectToBox(env, env.getMinY-100)

      createLinesFromEdges
      (q.size, createLinesFromEdges)
    }
  }

  def getParabola(currArc: Arc, yd: Double): LineString = {
    val parabola = ArrayBuffer[Coordinate]()
    val p = currArc.site.y - yd
    if(p == 0){
        parabola ++= Array(currArc.site, new Coordinate(currArc.site.x, yd))
    } else {
      for (x <- 0 until 1000) {
        val y = Math.pow((x) - currArc.site.x, 2) / (2 * p) + currArc.site.y - (p / 2)
        parabola += new Coordinate(x, y)
      }
    }
    factory.createLineString(parabola.toArray)
  }

  def run(points: Array[Coordinate]) = {
    for(i <- 0 until points.length){
      q.enqueue(new SiteEvent(points(i), points(i).y))
    }
    var lastY : Double = 0
    while(!q.isEmpty){
      val event = q.dequeue()
      event match {
        case e : SiteEvent => handleSiteEvent(e.site)
        case e : CircleEvent =>{
          //Tree.printTree(tree)
          //println(e.a.pred.get.site + " -- " + e.a.site + " -- " + e.a.next.get.site)
          handleCircleEvent(Tree.search(e.a,tree)(new NodeOrdering(e.y)), e.y)
        }
      }
      lastY = event.y
    }
    val multipoint = factory.createMultiPoint(points)
    val pointsV = edgeList.vertices.map(v => new Coordinate(v.point.x, v.point.y))
    val multipointV = factory.createMultiPoint(pointsV.toArray)
    val allPoints = multipointV.union(multipoint)
    val env = allPoints.getEnvelopeInternal
    val expandBy: Double = Math.max(env.getWidth, env.getHeight)
    env.expandBy(expandBy)
    val treeL = tree.toList


    connectToBox(env, lastY - 10)

    createLinesFromEdges
  }


  def distance(x1: Coordinate, x2: Coordinate): Double = {
    Math.sqrt(Math.pow(x1.x - x2.x, 2) + Math.pow(x1.y - x2.y, 2))
  }
  def connectToBox(box: Envelope, y: Double) = {
    def computeIntersections(p1 : Coordinate, p2 : Coordinate, env : Envelope) : (Coordinate, Coordinate) = {
      if (p2.y == p1.y){
        val b = (p1.x + p2.x)/2
        (new Coordinate(b, env.getMinY), new Coordinate(b, env.getMaxY))
      } else {
        val a = (p1.x - p2.x) / (p2.y - p1.y)
        val b = (Math.pow(p2.y, 2) + Math.pow(p2.x, 2) - Math.pow(p1.x, 2) - Math.pow(p1.y, 2)) / (2 * (p2.y - p1.y))
        (new Coordinate(env.getMinX, (a*env.getMinX)+b), new Coordinate(env.getMaxX, (a*env.getMaxX)+b))
      }
    }
    def choose(tuple: (Coordinate, Coordinate), x1: Coordinate, x2: Coordinate) = {
      val breakpoint = new NodeOrdering(y).breakPoint(tuple)
      val x1Distance = distance(x1, breakpoint)
      val x2Distance = distance(x2, breakpoint)

      if (x1Distance <= x2Distance) x1
      else x2
    }
    tree.toList.foreach {
      case node : SiteTuple => {
        val sites = node.sites

        val (x1, x2) = computeIntersections(sites._1, sites._2, box)
        val inter = choose(sites, x1, x2)
        if(node.edge.origin == null) {
          val orig = new Vertex(inter,node.edge)
          node.edge.origin = orig
        }

      }
    }
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
    val lines = edgeList.edges.filter { edge =>
      edge.origin != null && edge.twin.origin != null
    }.map { edge =>
        val p1 = edge.origin.point
        val p2 = edge.twin.origin.point

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
        //println("check a.pred : " + a.pred.get.pred.get.site + " -- " + a.pred.get.site + " -- " + a.pred.get.next.get.site)
        a.next.foreach(checkCircleEvent(_, sweepY)) //Check the triple of consecutive arcs where the a is the left arc
        //println("check a.next : " + a.next.get.pred.get.site + " -- " + a.next.get.site + " -- " + a.next.get.next.get.site)

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
      //val (h1, h2) = edgeList.createEdge


      val (old, newTree) = Tree.addParabola(newArc, tree, edgeList)(new NodeOrdering(p.y)) // create and add the subtree, link the half-edge with internal nodes, link newArc with pred/next
      tree = newTree
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
        val ord = new NodeOrdering(center.y - r)
        val b1 = ord.breakPoint((a.pred.get.site, a.site))
        val b2 = ord.breakPoint((a.site, a.next.get.site))
        if(round(b1.x) == round(b2.x)) {
          //add event
          val event = CircleEvent(a, center.y - r)

          a.event = Some(event)
          q += event
        }
      }
    }
  }
  def round(x: Double) = {
    Math.floor(x * 100) / 100
  }
  def computeCenter(a: Arc) : Option[Coordinate] = {
    a match {
      case Arc(site, Some(pred), Some(next), event) => {
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
      }
      case _ => None
    }
  }

}
