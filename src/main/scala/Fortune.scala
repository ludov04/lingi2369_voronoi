import com.vividsolutions.jts.geom.Point

import scala.collection.mutable

/**
 * Created by ludov on 27/04/15.
 */

class Arc {
  ???
}

trait Event

class CircleEvent extends Event
class SiteEvent extends Event

class Fortune {

  val q = new mutable.PriorityQueue[Event]()
  val edgeList : DCEL = new DCEL()
  val tree = new mutable.TreeSet[Arc]()

  def handleCircleEvent(a : Arc) = ???

  def handleSiteEvent(p: Point) = ???

}
