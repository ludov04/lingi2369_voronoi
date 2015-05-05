import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}

/**
 * Created by ludov on 5/05/15.
 */
trait Voronoi {
  val factory = new GeometryFactory()
  val points: Array[Coordinate]
}
