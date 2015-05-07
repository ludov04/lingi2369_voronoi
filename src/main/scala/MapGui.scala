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
 * AUTHORS :
 * Fabian Souris
 * Ludovic Vanoorenberghe
 */
class MapGui {

  val fact = new GeometryFactory()
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val x = (screenSize.getWidth-50).toInt
  val y = (screenSize.getHeight-50).toInt
  var fortuneS = new Fortune(Array[Coordinate](), x, y)
  val map = new JMapViewer()


  def addToMap(polygons : GeometryCollection) = {
    val env = polygons.getEnvelopeInternal
    val polygonArray = (0 until polygons.getNumGeometries).map(polygons.getGeometryN(_)).filter { polygon =>
      polygon.getCoordinates.map { c =>
        c.x != env.getMaxX && c.x != env.getMinX && c.y != env.getMaxY && c.y != env.getMinY
      }.reduceLeft(_ && _)
    }
    val areas = polygonArray.map(_.getArea)
    val colors = (0 until polygonArray.length).map( i => {
      val colVal = (((areas(i)-areas.min)/(areas.max-areas.min))*255).toInt
      new Color(255-colVal, colVal, 0, 150)
    })
    for(i <- 0 until polygonArray.length) {
      val pol = polygonArray(i)
      val polPoints = pol.getCoordinates
      val points = polPoints.map { c =>
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
    val textField = new JTextField(20);
    val naiveButton = new JButton("Naive")
    val autoStepButton = new JButton("Auto Step")
    val stepButton = new JButton("Step")
    val fortuneButton = new JButton("Fortune")
    val genButton = new JButton("Add")
    val clearButton = new JButton("Clear")
    buttons.add(autoStepButton)
    buttons.add(stepButton)
    buttons.add(fortuneButton)
    buttons.add(naiveButton)
    buttons.add(genButton)
    buttons.add(clearButton)
    buttons.add(textField)
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

        map.removeAllMapMarkers()
        map.removeAllMapPolygons()
        val x = Util.read(textField.getText.split(",").map(_.trim).toList)
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