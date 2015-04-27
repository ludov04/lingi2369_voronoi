import com.vividsolutions.jts.geom.{Coordinate, Point}

import scala.collection.mutable

/**
 * Created by ludov on 27/04/15.
 */

class Arc(site: Coordinate) {

}

trait Event

class CircleEvent extends Event
class SiteEvent extends Event

class Fortune {

  val q = new mutable.PriorityQueue[Event]()
  val edgeList : DCEL = new DCEL()
  val tree = new mutable.TreeSet[Arc]()

  def handleCircleEvent(a : Arc) = {
    val l = tree.
  }

  def handleSiteEvent(p: Coordinate) = ???

  def checkCircleEvent(a : Arc) = ???

}
