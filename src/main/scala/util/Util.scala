package util

import com.vividsolutions.jts.geom.Coordinate

/**
 * Created by ludov on 5/05/15.
 */
object Util {
  def round(x: Double) = {
    Math.floor(x * 100) / 100
  }

  def distance(x1: Coordinate, x2: Coordinate): Double = {
    Math.sqrt(Math.pow(x1.x - x2.x, 2) + Math.pow(x1.y - x2.y, 2))
  }
}
