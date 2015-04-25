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

import com.vividsolutions.jts.io.WKTReader

class Gui(val content : Drawer) {

  def show() {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    content.setPreferredSize(new Dimension(1000, 600))
    frame.getContentPane.add(content, BorderLayout.CENTER)

    frame.pack()
    frame.setVisible(true)

  }

}

object GuiRun {
  def main(args: Array[String]): Unit = {
    val rdr: WKTReader = new WKTReader
    val points = rdr.read("MULTIPOINT ((150 290), (370 120), (100 170), (330 370), (190 60))")
    val naive = new Naive(points)
    val draw = new Drawer(naive.run())
    val gui = new Gui(draw)
    gui.show
  }
}