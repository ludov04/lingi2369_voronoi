package util

import java.io.File

import com.vividsolutions.jts.geom.Coordinate
import org.openstreetmap.gui.jmapviewer.{Coordinate => MapCoordinate}
import com.github.tototoshi.csv._
/**
 * Created by ludov on 5/05/15.
 */
object Util {
  def round(x: Double) = {
    Math.floor(x * 10000) / 10000
  }

  def distance(x1: Coordinate, x2: Coordinate): Double = {
    Math.sqrt(Math.pow(x1.x - x2.x, 2) + Math.pow(x1.y - x2.y, 2))
  }

  def degreeToDecimal(degree : (Int, Double, Double)) : Double = {
    degree._1 + degree._2 /60  + degree._3/3600
  }

  def read(communes: List[String]) = {
    getClass.getResource("list.csv")
    val reader = CSVReader.open(new File("/Users/ludov/dev/lingi2369_voronoi_2/src/main/resources/list.csv"))
    val values = reader.allWithHeaders()
    values.filter(_.get("Com").fold(false)(commune => communes.map(commune.contains(_)).reduceLeft(_ || _) ))flatMap { row =>
      val lon = row.get("Lon")
      val lonDec = lon.map { l =>
        val l1 = l.split("°")
        val deg = l1(0).toInt


        val minSec = l1(1).split("'")
        val min = minSec(0).toInt
        val sec = minSec(1).split("''")

        val secV = sec(0).toDouble
        degreeToDecimal((deg, min, secV))
      }

      val lat = row.get("Lat")
      val latDec = lat.map { l =>
        val l1 = l.split("°")
        val deg = l1(0).toInt


        val minSec = l1(1).split("'")
        val min = minSec(0).toInt
        val sec = minSec(1).split("''")

        val secV = sec(0).toDouble
        degreeToDecimal((deg, min, secV))
      }

      for {
        latitude <- latDec
        longitude <- lonDec
      } yield new MapCoordinate(latitude, longitude)

    }
  }


}
