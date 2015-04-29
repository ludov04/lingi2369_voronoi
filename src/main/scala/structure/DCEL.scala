package structure

import com.vividsolutions.jts.geom.Coordinate

import scala.collection.mutable
/**
 * Created by ludov on 27/04/15.
 */
object DCEL {
  val edges = new mutable.ListBuffer[HalfEdge]()
  val faces = new mutable.ListBuffer[Face]()
  val vertices = new mutable.ListBuffer[Vertex]()

  case class Face(edge : HalfEdge)

  case class Vertex(point: Coordinate, leaving : HalfEdge)

  case class HalfEdge(var origin : Vertex,
                      var twin: HalfEdge,
                      var face : Face,
                      var next : HalfEdge,
                      var prev: HalfEdge)


  def createEdge : (HalfEdge, HalfEdge) = {
    val h1 = HalfEdge(null, null, null, null, null)
    val h2 = HalfEdge(null, null, null, null, null)
    h1.twin = h2
    h2.twin = h1
    edges += h1
    edges += h2
    (h1, h2)
  }
}
