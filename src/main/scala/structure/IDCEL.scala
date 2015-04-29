package structure

/**
 * Created by ludov on 27/04/15.
 */
trait IDCEL[P] {

  protected val edges : Set[HalfEdge]
  protected val faces : Set[Face]
  protected val vertices : Set[Vertex]


  case class Face(edge : HalfEdge) {

  }

  case class HalfEdge(var origin : Vertex,
                      var twin: HalfEdge,
                      var face : Face,
                      var next : HalfEdge,
                      var prev: HalfEdge)



  case class Vertex(point: P, leaving : HalfEdge) {


  }

  def addHalfEdges(v1 : Vertex, v2: Vertex)

}
