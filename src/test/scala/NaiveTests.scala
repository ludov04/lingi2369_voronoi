import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.triangulate.VoronoiDiagramBuilder
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by ludov on 30/04/15.
 */


abstract class NaiveSpec extends FlatSpec with Matchers


class VertexSpec extends NaiveSpec {

  "Naive" should "should return the same set of vertex as jts" in {
    val factory = new GeometryFactory()
    val rdr: WKTReader = new WKTReader
    val points = rdr.read("MULTIPOINT ((150 290), (370 120), (100 170), (330 370), (190 60))")
    val naive = new Naive(points.getCoordinates)
    val diagram = naive.run
    val pointsNaive = diagram.getCoordinates

    val jtsVoronoi : VoronoiDiagramBuilder = new VoronoiDiagramBuilder()
    jtsVoronoi.setSites(points)
    val jtsDiagram = jtsVoronoi.getDiagram(factory)
    val jtsCoords = jtsDiagram.getCoordinates

    jtsCoords.length should equal (pointsNaive.length)
  }

  // What happens if the right neighbor and the left neighbor correspond to the same site ?
  // Should not happen but who nows ?

}
