package structure

import com.vividsolutions.jts.geom.Coordinate

import scala.collection.mutable
/**
 * Created by ludov on 27/04/15.
 */
class DCEL extends IDCEL[Coordinate] {
  import DCEL._
  val edges = new mutable.HashSet[HalfEdge]()
  val faces = new mutable.HashSet()
  val vertices = new mutable.HashSet()

  override def addHalfEdges(v1: Vertex, v2: Vertex): Unit = {

  }

  def createHalfEdges : (HalfEdge, HalfEdge) = {
    val h1 = HalfEdge(null, null, null, null, null)
    val h2 = HalfEdge(null, null, null, null, null)
    h1.twin = h2
    h2.twin = h1
    edges.add(h1)
    edges.add(h2)
    (h1, h2)
  }
}

object DCEL extends DCEL