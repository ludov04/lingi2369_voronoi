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

class Drawer(val content: GeometryCollection) extends JComponent {

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    for(i <- 0 until content.getNumGeometries){
      val currGeometry = content.getGeometryN(i)

    }
  }

}
