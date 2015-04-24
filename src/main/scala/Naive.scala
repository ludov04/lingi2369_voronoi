/**
 * Created by ludov on 24/04/15.
 */

import com.vividsolutions.jts.geom.impl.CoordinateArraySequence
import com.vividsolutions.jts.geom._
import language.implicitConversions
import Array._


object Naive {

  val factory = new GeometryFactory()

  var myMatrix = ofDim[LineSegment](3,3)


  implicit def envelopeToLinearRing(env : Envelope) : LinearRing = {
    val coordinatesArray = Array(
      new Coordinate(env.getMinX, env.getMaxY, 0),
      new Coordinate(env.getMaxX, env.getMaxY, 0),
      new Coordinate(env.getMaxX, env.getMinY, 0),
      new Coordinate(env.getMaxX, env.getMinY, 0)
    )
    val coordinatesSeq : CoordinateArraySequence = new CoordinateArraySequence(coordinatesArray)

    new LinearRing(coordinatesSeq, factory)
  }



  def segmentMatrix(points: MultiPoint, env : Envelope) = {
    val coordinates = points.getCoordinates
    val length = coordinates.length
    var matrix = ofDim[LineSegment](length, length)
    for (x <- 0 until length;
         y <- 0 until length if y < x)
    {
      myMatrix(x)(y) = computeBisector(factory.createPoint(coordinates(x)),factory.createPoint(coordinates(y)), env);
    }
    ??? 
  }

  def computeBisector(p1 : Point, p2 : Point, env : Envelope) : LineSegment = {
    if (p2.getY == p1.getY){
      val b = (p1.getX + p2.getX)/2
      new LineSegment(env.getMinX, b, env.getMaxX, b)
    } else {
      val a = (p1.getX - p2.getX) / (p2.getY - p1.getY)
      val b = (Math.pow(p2.getY, 2) + Math.pow(p2.getX, 2) - Math.pow(p1.getX, 2) - Math.pow(p1.getY, 2)) / (2 * (p2.getY - p1.getY))
      new LineSegment(env.getMinX, (a*env.getMinX)+b, env.getMaxX, (a*env.getMaxX)+b)
    }
  }


  def computePolygon(p : Point, b : LineSegment, env : Envelope) : Polygon = {
    val mainPol = new Polygon(envelopeToLinearRing(env), Array[LinearRing](), factory)
    val pol1 = {
      if (b.p0.y < env.getMinY) {
        new Polygon(
          new LinearRing(
            new CoordinateArraySequence(
              Array(b.p0, new Coordinate(env.getMaxX, env.getMinY, 0), b.p1)),
            factory),
          Array[LinearRing](), factory)
      } else if (b.p1.y < env.getMinY) {

      } else {

      }
    }
  }



}