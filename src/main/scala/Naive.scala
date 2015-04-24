/**
 * Created by ludov on 24/04/15.
 */

import java.util.Collection

import com.vividsolutions.jts.geom.impl.CoordinateArraySequence
import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.operation.polygonize.Polygonizer
import language.implicitConversions
import Array._
import scala.collection.JavaConversions._


class Naive(points: MultiPoint) {

  val factory = new GeometryFactory()
  val n : Int = points.getNumGeometries()
  val segmentMatrix: Array[Array[LineSegment]] = ofDim[LineSegment](n, n)
  val envelope = points.getEnvelopeInternal


  implicit def envelopeToLinearRing(env : Envelope) : LinearRing = {
    val coordinatesArray = Array(
      new Coordinate(env.getMinX, env.getMaxY, 0),
      new Coordinate(env.getMaxX, env.getMaxY, 0),
      new Coordinate(env.getMaxX, env.getMinY, 0),
      new Coordinate(env.getMaxX, env.getMinY, 0),
      new Coordinate(env.getMinX, env.getMaxY, 0) // must be closed (closed mean last point is equal to first)
    )


    factory.createLinearRing(coordinatesArray)
  }




  def computeSegmentMatrix(env : Envelope) = {
    val coordinates = points.getCoordinates

    for (x <- 0 until n;
         y <- 0 until n if y < x)
    {
      segmentMatrix(x)(y) = computeBisector(factory.createPoint(coordinates(x)),factory.createPoint(coordinates(y)), env)
    }
    segmentMatrix
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

    val mainPol = factory.createPolygon(env)
    val lines = factory.createGeometryCollection(Array(env, b.toGeometry(factory))).union()

    val polygonizer = new Polygonizer()
    polygonizer.add(lines)
    val polygons =  polygonizer.getPolygons().asInstanceOf[(Collection[Polygon])]

    val p = (for (polygon <- polygons if(polygon.contains(p))) yield polygon).head

    p

  }

  def computeVoronoiCell(p: Int) = {
    val polygons = Array[Geometry]()
    val coordinates = points.getCoordinates
    val point = factory.createPoint(coordinates(p))

    for (x <- 0 until n if p != x) {
      val other = factory.createPoint(coordinates(x))
      val bisector = computeBisector(point, other, envelope)
      polygons :+ computePolygon(point, bisector, envelope)
    }
    val collection = factory.createGeometryCollection(polygons)
    collection.union()
  }

  def run() = {
    for
  }


}
