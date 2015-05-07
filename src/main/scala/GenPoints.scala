import com.vividsolutions.jts.geom.Coordinate

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Fabian on 26-04-15.
 */
object GenPoints {
  def generate(x : Int, y : Int, n : Int): Array[Coordinate] = {
    val points = new ArrayBuffer[Coordinate]()
    for(i <- 0 until n){
      points += new Coordinate(Math.random()*x, Math.random()*y, 0)
    }
    points.toArray
  }
}
