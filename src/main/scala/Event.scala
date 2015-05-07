import com.vividsolutions.jts.geom.Coordinate

/**
 * Created by ludov on 5/05/15.
 */

trait Event {
  def y : Double
}

case class CircleEvent(a: Arc, y: Double) extends Event
case class SiteEvent(site: Coordinate, y: Double) extends Event