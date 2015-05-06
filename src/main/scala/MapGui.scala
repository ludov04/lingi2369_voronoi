import java.awt.event.{MouseEvent, MouseListener, ActionEvent, ActionListener}
import java.awt.{Toolkit, BorderLayout, Dimension}
import javax.swing._
import scala.collection.JavaConversions._

import com.vividsolutions.jts.geom.{GeometryCollection, Geometry, Coordinate, GeometryFactory}
import org.openstreetmap.gui.jmapviewer.{MapMarkerDot, MapPolygonImpl, Coordinate => MapCoordinate, JMapViewer}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ludov on 6/05/15.
 */
class MapGui {

  val points = new ArrayBuffer[Coordinate]()
  val fact = new GeometryFactory()
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val x = (screenSize.getWidth-50).toInt
  val y = (screenSize.getHeight-50).toInt
  var fortuneS = new Fortune(points.toArray, x, y)
  val map = new JMapViewer()


  def addToMap(lines : GeometryCollection) = {
    for(i <- 0 until lines.getNumGeometries
    ) {
      val line = lines.getGeometryN(i)
      val linePoints = line.getCoordinates
      val points = linePoints.map { c =>
        new MapCoordinate(c.y, c.x)
      }

      val workaround = points.+:(points.last)

      map.addMapPolygon(new MapPolygonImpl(workaround.toList))
    }
  }


  def show() {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    frame.getContentPane.add(map, BorderLayout.CENTER)



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


    val timer = new Timer(50, null)
    val stepListener = new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val result = if(fortuneS.runStep()) {
          timer.stop()
          fortuneS.computeDiagram()
        }
        else fortuneS.getBeachLine
      }
    }

    timer.addActionListener(stepListener)
    stepButton.addActionListener(stepListener)



    autoStepButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        timer.start()
      }
    })

    fortuneButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {

        val fortune = new Fortune(points.toArray, x, y)
        val result = fortune.run
        map.removeAllMapPolygons()
        addToMap(result)

      }
    })

    naiveButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val naive = new Naive(points.toArray)
        map.removeAllMapPolygons()
        addToMap(naive.run)
      }
    })

    genButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val n = 100
        val newP = GenPoints.generate(x, y, n)
        for(i <- 0 until n) points += newP(i)
        fortuneS = new Fortune(points.toArray, x, y)
      }
    })

    clearButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        points.clear()
        map.removeAllMapMarkers()
        map.removeAllMapPolygons()
        fortuneS = new Fortune(points.toArray, x, y)
      }
    })

    val mapListener = map.getMouseListeners()(0)
    val mapMotionListener = map.getMouseMotionListeners()(0)
    val mouseListener = new VoronoiListener()


    //map.getOverlayPanel.setVisible(true)
    map.addMouseListener(mouseListener)
    frame.pack()
    frame.setVisible(true)

  }

  private class VoronoiListener extends MouseListener {
    override def mouseExited(e: MouseEvent): Unit = {

    }

    override def mouseClicked(e: MouseEvent): Unit = {
      val c =  map.getPosition(e.getX, e.getY)
      points += new Coordinate(c.getLon, c.getLat)
      val marker = new MapMarkerDot(c)
      map.addMapMarker(marker)
    }

    override def mouseEntered(e: MouseEvent): Unit = {

    }

    override def mousePressed(e: MouseEvent): Unit = {

    }

    override def mouseReleased(e: MouseEvent): Unit = {

    }
  }

}


object MapGuiRun {
  def main(args: Array[String]): Unit = {
    val gui = new MapGui
    gui.show
  }
}