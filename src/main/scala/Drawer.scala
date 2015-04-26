/**
 * Created by Fabian on 25-04-15.
 */

import java.awt.BorderLayout
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.LinkedList

import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JFrame
import javax.swing.JPanel

import com.vividsolutions.jts.geom._

class Drawer(var points : Array[Coordinate], var result: GeometryCollection) extends JComponent {

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    for(i <- 0 until result.getNumGeometries){
      val currGeometry = result.getGeometryN(i)
      currGeometry.getGeometryType match {
        case "Polygon" => {
          val coords = currGeometry.getCoordinates
          for (j <- 0 until coords.length) {
            if (j == coords.length - 1) g.drawLine(coords(j).x.toInt, coords(j).y.toInt, coords(0).x.toInt, coords(0).y.toInt)
            else g.drawLine(coords(j).x.toInt, coords(j).y.toInt, coords(j + 1).x.toInt, coords(j + 1).y.toInt)
          }
        }
        case "Point" => {
          g.drawLine(currGeometry.getCoordinate.x.toInt, currGeometry.getCoordinate.y.toInt, currGeometry.getCoordinate.x.toInt, currGeometry.getCoordinate.y.toInt)
        }
        case "MultiPoint" => {
          val coords = currGeometry.getCoordinates
          for (j <- 0 until coords.length) {
            g.fillOval(coords(j).x.toInt - 2, coords(j).y.toInt - 2, 4, 4)
          }
        }
      }
    }
    for (j <- 0 until points.length) {
      g.setColor(Color.RED)
      g.fillOval(points(j).x.toInt - 2, points(j).y.toInt - 2, 4, 4)
    }
  }

  def refresh(): Unit ={
    repaint()
  }
  def refresh(newR : GeometryCollection): Unit ={
    this.result = newR
    repaint()
  }
  def refresh(newP : Array[Coordinate]): Unit = {
    this.points = newP
    repaint()
  }
    def refresh(newP : Array[Coordinate], newR : GeometryCollection): Unit ={
      this.points = newP
      this.result = newR
      repaint()
  }

}
