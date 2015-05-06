import java.awt.event.{MouseEvent, MouseListener, ActionEvent, ActionListener}
import java.awt._
import javax.swing._
import org.openstreetmap.gui.jmapviewer.interfaces.MapMarker
import util.Util

import scala.collection.JavaConversions._

import com.vividsolutions.jts.geom.{GeometryCollection, Geometry, Coordinate, GeometryFactory}
import org.openstreetmap.gui.jmapviewer.{Coordinate => MapCoordinate, _}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ludov on 6/05/15.
 */
class MapGui {

  val fact = new GeometryFactory()
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val x = (screenSize.getWidth-50).toInt
  val y = (screenSize.getHeight-50).toInt
  var fortuneS = new Fortune(Array[Coordinate](), x, y)
  val map = new JMapViewer()


  def addToMap(lines : GeometryCollection) = {
    val areas = (0 until lines.getNumGeometries).map(lines.getGeometryN(_).getArea)
    val colors = (0 until lines.getNumGeometries).map( i => {
      val colVal = (((areas(i)-areas.min)/(areas.max-areas.min))*255).toInt
      new Color(colVal, 255-colVal, 0, 70)
    })
    for(i <- 0 until lines.getNumGeometries) {
      val line = lines.getGeometryN(i)
      val linePoints = line.getCoordinates
      val points = linePoints.map { c =>
        val cMap = map.getPosition(c.x.toInt, c.y.toInt)
        new MapCoordinate(cMap.getLat, cMap.getLon)
      }

      val workaround = points.+:(points.last)
      map.addMapPolygon(new MapPolygonImpl(null, null, workaround.toList, new Style(Color.BLUE, colors(i), new BasicStroke(2), MapObjectImpl.getDefaultFont)))
    }
  }

  def transformToLinearSystem(markers: java.util.List[MapMarker]) = {
    markers.map { point =>
      val pos = map.getMapPosition(point.getCoordinate, false)
      new Coordinate(pos.x, pos.y)
    }.toArray
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
        if(timer.isRunning) timer.stop()
        else timer.start()
      }
    })

    fortuneButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val toAdd = transformToLinearSystem(map.getMapMarkerList)
        val fortune = new Fortune(toAdd.toArray, x, y)
        val result = fortune.run
        map.removeAllMapPolygons()
        addToMap(result)

      }
    })

    naiveButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val points = transformToLinearSystem(map.getMapMarkerList)
        val naive = new Naive(points.toArray)
        map.removeAllMapPolygons()
        addToMap(naive.run)
      }
    })

    genButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        /*val n = 100
        val newP = GenPoints.generate(x, y, n)
        newP.foreach { e =>
          val c =  map.getPosition(e.x.toInt, e.y.toInt)
          val marker = new MapMarkerDot(c)
          map.addMapMarker(marker)
        }*/

        val x = Util.read("1000 Bruxelles")
        x.foreach { e =>
          val marker = new MapMarkerDot(e)
          map.addMapMarker(marker)
        }
      }
    })

    clearButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        map.removeAllMapMarkers()
        map.removeAllMapPolygons()
        fortuneS = new Fortune(Array[Coordinate](), x, y)
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