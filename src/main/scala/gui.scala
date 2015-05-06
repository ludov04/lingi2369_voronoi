/**
 * Created by Fabian on 25-04-15.
 */

import java.awt.{Toolkit, BorderLayout, Dimension}
import java.awt.event.{MouseEvent, MouseListener, ActionEvent, ActionListener}

import javax.swing._

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.io.WKTReader
import structure.DCEL
import sun.nio.cs.Surrogate.Generator

import scala.collection.mutable.ArrayBuffer

class Gui(val content : Drawer) {

  val points = new ArrayBuffer[Coordinate]()
  val fact = new GeometryFactory()
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val x = (screenSize.getWidth-20).toInt
  val y = (screenSize.getHeight-150).toInt
  var nStep = 0
  var fortuneS = new Fortune(points.toArray, x, y)
  var q = 1



  def show() {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    content.setPreferredSize(new Dimension(x, y))
    content.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    frame.getContentPane.add(content, BorderLayout.CENTER)

    val buttons = new JPanel()
    val naiveButton = new JButton("Naive")
    val autoStepButton = new JButton("Auto Step")
    val stepButton = new JButton("Step")
    val fortuneButton = new JButton("Fortune")
    val genButton = new JButton("Generate")
    val clearButton = new JButton("Clear")
    buttons.add(autoStepButton)
    buttons.add(stepButton)
    buttons.add(fortuneButton)
    buttons.add(naiveButton)
    buttons.add(genButton)
    buttons.add(clearButton)
    frame.getContentPane.add(buttons, BorderLayout.SOUTH)


    val timer = new Timer(20, null)
    val stepListener = new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val result = if(fortuneS.runStep()) {
          timer.stop()
          fortuneS.computeDiagram()
        }
        else {
          fortuneS.getBeachLine.union(fortuneS.computeStepDiagram).asInstanceOf[GeometryCollection]
        }
        content.refresh(points.toArray,  result )

      }
    }

    timer.addActionListener(stepListener)
    stepButton.addActionListener(stepListener)



    autoStepButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if(timer.isRunning) timer.stop()
        else timer.start()
      }
    })

    fortuneButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val fortune = new Fortune(points.toArray, x, y)
        content.refresh(fortune.run)
      }
    })

    naiveButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val naive = new Naive(points.toArray)
        content.refresh(naive.run)
      }
    })

    genButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val n = 100
        val newP = GenPoints.generate(x, y, n)
        for(i <- 0 until n) points += newP(i)
        fortuneS = new Fortune(points.toArray, x, y)
        content.refresh(points.toArray)
      }
    })

    clearButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        fortuneS = new Fortune(points.toArray, x, y)
        nStep = 0
        points.clear()
        content.refresh(points.toArray, fact.createGeometryCollection(Array[Geometry]()))
      }
    })

    content.addMouseListener(new MouseListener {
      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {
        points += new Coordinate(e.getX, e.getY)
        fortuneS = new Fortune(points.toArray, x, y)
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
    //val points = rdr.read("MULTILINESTRING ((136 225, 185 335, 330 270), (150 180, 164 180, 176 180, 188 180, 202 180, 214 180, 224 180, 237 180, 249 180, 260 180))")
    //val naive = new Naive(points)
    val fact = new GeometryFactory()
    val draw = new Drawer(Array[Coordinate](), fact.createGeometryCollection(Array[Geometry]()))
    val gui = new Gui(draw)
    gui.show
  }
}