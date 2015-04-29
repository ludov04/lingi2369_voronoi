package structure

import com.vividsolutions.jts.geom.Coordinate

import scala.collection.mutable
/**
 * Created by ludov on 27/04/15.
 */
class DCEL extends IDCEL[Coordinate] {
  val edges = new mutable.HashSet()
  val faces = new mutable.HashSet()
  val vertices = new mutable.HashSet()

  override def addHalfEdges(v1: Vertex, v2: Vertex): Unit = {

  }
}

object DCEL extends DCEL