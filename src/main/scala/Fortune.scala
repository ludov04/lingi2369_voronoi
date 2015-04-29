import com.vividsolutions.jts.geom.{Coordinate, Point}
import structure.DCEL

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
  val tree = EmptyT()

  def handleCircleEvent(a : Arc) = {

  }

  def handleSiteEvent(p: Coordinate) = {
    if (tree.isEmpty) {
      Tree.insert(new Arc(p, None, None, None), tree)
    }
    else {
      val above : Leaf = Tree.search(p, tree) // the leaf containing the arc vertically above p

    }
  }

  def checkCircleEvent(a : Arc) = ???

}
