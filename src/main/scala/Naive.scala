/**
 * Created by ludov on 24/04/15.
 */

import com.vividsolutions.jts.geom._

object Naive {


  def computeBisector(p1 : Point, p2 : Point, env : Envelope) = {
    if (p2.getY - p1.getY == 0){
      val b = (p1.getX + p2.getX)/2
      new LineSegment(env.getMinX, b, env.getMaxX, b)
    } else {
      val a = (p1.getX - p2.getX) / (p2.getY - p1.getY)
      val b = (Math.pow(p2.getY, 2) + Math.pow(p2.getX, 2) - Math.pow(p1.getX, 2) - Math.pow(p1.getY, 2)) / (2 * (p2.getY - p1.getY))
      new LineSegment(env.getMinX, (a*env.getMinX)+b, env.getMaxX, (a*env.getMaxX)+b)
    }
  }

  def computePolygon(p : Point, b : LineSegment, env : Envelope) = {
    val mainPol = new Polygon()
  }


}
