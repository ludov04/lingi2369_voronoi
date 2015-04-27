import com.vividsolutions.jts.geom.{Coordinate, Point}

import scala.collection.mutable
import scalacaster.Tree
import scalacaster.Tree._
/**
 * Created by ludov on 27/04/15.
 */

trait Event

class CircleEvent extends Event
class SiteEvent extends Event

class Fortune {

  val q = new mutable.PriorityQueue[Event]()
  val edgeList : DCEL = new DCEL()
  val tree = Tree.empty[Arc]

  def handleCircleEvent(a : Arc) = {

  }

  def handleSiteEvent(p: Coordinate) = {
    if (tree.isEmpty) {
      tree.add(new Arc(p, None, None, None))
    }
    else {
      val above : Arc = ??? // the arc vertically above p

    }
  }

  def checkCircleEvent(a : Arc) = ???

}
