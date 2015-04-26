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

import com.vividsolutions.jts.geom.{Polygon, GeometryCollection}

class Drawer(var content: GeometryCollection) extends JComponent {

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    for(i <- 0 until content.getNumGeometries){
      val currGeometry = content.getGeometryN(i)
      currGeometry.getGeometryType match {
        case "Polygon" => {
          val coords = currGeometry.getCoordinates
          for(j <- 0 until coords.length){
            if(j == coords.length-1) g.drawLine(coords(j).x.toInt, coords(j).y.toInt, coords(0).x.toInt, coords(0).y.toInt)
            else g.drawLine(coords(j).x.toInt, coords(j).y.toInt, coords(j+1).x.toInt, coords(j+1).y.toInt)
          }
        }
        case "Point" => {
          g.drawLine(currGeometry.getCoordinate.x.toInt, currGeometry.getCoordinate.y.toInt, currGeometry.getCoordinate.x.toInt, currGeometry.getCoordinate.y.toInt)
        }
        case "MultiPoint" => {
          val coords = currGeometry.getCoordinates
          println(coords(0).x.toInt)
          for(j <- 0 until coords.length){
            g.fillOval(coords(j).x.toInt-2, coords(j).y.toInt-2, 4, 4)
          }
        }
      }

    }
  }

  def refresh(): Unit ={
    repaint()
  }
  def refresh(newC : GeometryCollection): Unit ={
    this.content = newC
    repaint()
  }

}
