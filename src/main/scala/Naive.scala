/**
 * Created by ludov on 24/04/15.
 */

import java.util.Collection

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.io.WKTReader
import com.vividsolutions.jts.operation.polygonize.Polygonizer
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder

import scala.Array._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions


class Naive(val points: Array[Coordinate]) extends Voronoi {

  val n : Int = points.length
  val segmentMatrix: Array[Array[LineSegment]] = ofDim[LineSegment](n, n)
  val envelope = DelaunayTriangulationBuilder.envelope(points.toList)


  implicit def envelopeToLinearRing(env : Envelope) : LinearRing = {
    val coordinatesArray = Array(
      new Coordinate(env.getMinX, env.getMaxY, 0),
      new Coordinate(env.getMaxX, env.getMaxY, 0),
      new Coordinate(env.getMaxX, env.getMinY, 0),
      new Coordinate(env.getMinX, env.getMinY, 0),
      new Coordinate(env.getMinX, env.getMaxY, 0) // must be closed (closed mean last point is equal to first)
    )
    factory.createLinearRing(coordinatesArray)
  }


  def computeSegmentMatrix(env : Envelope) = {
    val coordinates = points

    for (x <- 0 until n;
         y <- 0 until n if y < x)
    {
      segmentMatrix(x)(y) = computeBisectorSegment(coordinates(x), coordinates(y), env)
    }
    segmentMatrix
  }

  def computeBisectorSegment(p1 : Coordinate, p2 : Coordinate, env : Envelope) : LineSegment = {
    val b = computeBisector(p1, p2, env)
    new LineSegment(b._1, b._2)
  }


  def computePolygon(p : Coordinate, b : LineSegment, env : Envelope) : Polygon = {

    val mainPol = factory.createPolygon(env)
    val coll = factory.createGeometryCollection(Array(env, b.toGeometry(factory)))
    val lines = coll.union()

    val polygonizer = new Polygonizer()
    polygonizer.add(lines)
    val polygons =  polygonizer.getPolygons.asInstanceOf[(Collection[Polygon])]

    (for (polygon <- polygons if polygon.contains(factory.createPoint(p))) yield polygon).head

  }

  def computeVoronoiCell(p: Int) = {
    var cell = factory.toGeometry(envelope)
    val coordinates = points
    val point = coordinates(p)

    for (x <- 0 until n if p != x) {
      val other = coordinates(x)
      val bisector = computeBisectorSegment(point, other, envelope)
      cell = cell.intersection(computePolygon(point, bisector, envelope))
    }
    cell

  }

  def run : GeometryCollection = {

    val cells = new ArrayBuffer[Geometry]()
    val expandBy: Double = Math.max(envelope.getWidth, envelope.getHeight)
    envelope.expandBy(1)
    computeSegmentMatrix(envelope)
    for( i <- 0 until n) {
      cells += computeVoronoiCell(i)
    }
    //cells += points //Not necessary since Points and Polygons are separated
    val diagram = factory.createGeometryCollection(cells.toArray)
    diagram
  }

}

object NaiveRun {
  def main(args: Array[String]) {
    val factory = new GeometryFactory()

    val rdr: WKTReader = new WKTReader
    val points = rdr.read("MULTIPOINT ((150 290), (370 120), (100 170), (330 370), (190 60))")
    val naive = new Naive(points.getCoordinates)
    println(factory.createMultiPoint(naive.run.getCoordinates).toText)
  }
}