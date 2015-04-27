/**
 * Created by Fabian on 25-04-15.
 */

import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.event.{MouseEvent, MouseListener, ActionEvent, ActionListener}

import javax.swing.{BorderFactory, JButton, JFrame, JPanel}

import com.vividsolutions.jts.geom._
import sun.nio.cs.Surrogate.Generator

import scala.collection.mutable.ArrayBuffer

class Gui(val content : Drawer) {

  val points = new ArrayBuffer[Coordinate]()
  val fact = new GeometryFactory()
  val x = 1000
  val y = 600

  def show() {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    content.setPreferredSize(new Dimension(x, y))
    content.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    frame.getContentPane.add(content, BorderLayout.CENTER)

    val buttons = new JPanel()
    val naiveButton = new JButton("Naive")
    val genButton = new JButton("Generate")
    val clearButton = new JButton("Clear")
    buttons.add(naiveButton)
    buttons.add(genButton)
    buttons.add(clearButton)
    frame.getContentPane.add(buttons, BorderLayout.SOUTH)

    naiveButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val naive = new Naive(fact.createMultiPoint(points.toArray))
        content.refresh(naive.run())
      }
    })

    genButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val n = 100
        val newP = GenPoints.generate(x, y, n)
        for(i <- 0 until n) points += newP(i)
        content.refresh(points.toArray)
      }
    })

    clearButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        points.clear()
        content.refresh(points.toArray, fact.createGeometryCollection(Array[Geometry]()))
      }
    })

    content.addMouseListener(new MouseListener {
      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {
        points += new Coordinate(e.getX, e.getY)
        content.refresh(points.toArray)
      }

      override def mouseEntered(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {}

      override def mouseReleased(e: MouseEvent): Unit = {}
    })

    frame.pack()
    frame.setVisible(true)

  }

}

object GuiRun {
  def main(args: Array[String]): Unit = {
    //val rdr: WKTReader = new WKTReader
    //val points = rdr.read("MULTIPOINT ((150 290), (370 120), (100 170), (330 370), (190 60))")
    //val naive = new Naive(points)
    val fact = new GeometryFactory()
    val draw = new Drawer(Array[Coordinate](), fact.createGeometryCollection(Array[Geometry]()))
    val gui = new Gui(draw)
    gui.show
  }
}