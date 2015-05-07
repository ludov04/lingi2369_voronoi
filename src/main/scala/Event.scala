import com.vividsolutions.jts.geom.Coordinate

/**
 * AUTHORS :
 * Fabian Souris
 * Ludovic Vanoorenberghe
 */

trait Event {
  def y : Double
}

case class CircleEvent(a: Arc, y: Double) extends Event
case class SiteEvent(site: Coordinate, y: Double) extends Event