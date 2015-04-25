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

import com.vividsolutions.jts.geom.GeometryCollection

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
