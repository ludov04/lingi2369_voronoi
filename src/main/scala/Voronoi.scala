import com.vividsolutions.jts.geom._

/**
 * AUTHORS :
 * Fabian Souris
 * Ludovic Vanoorenberghe
 */
trait Voronoi {
  val factory = new GeometryFactory()
  val points: Array[Coordinate]
  def run : GeometryCollection

  def computeBisector(p1 : Coordinate, p2 : Coordinate, env : Envelope) : (Coordinate, Coordinate) = {
    if (p2.y == p1.y){
      val b = (p1.x + p2.x)/2
      (new Coordinate(b, env.getMinY), new Coordinate(b, env.getMaxY))
    } else {
      val a = (p1.x - p2.x) / (p2.y - p1.y)
      val b = (Math.pow(p2.y, 2) + Math.pow(p2.x, 2) - Math.pow(p1.x, 2) - Math.pow(p1.y, 2)) / (2 * (p2.y - p1.y))
      (new Coordinate(env.getMinX, (a*env.getMinX)+b), new Coordinate(env.getMaxX, (a*env.getMaxX)+b))
    }
  }
}
