/**
 * AUTHORS :
 * Fabian Souris
 * Ludovic Vanoorenberghe
 */

import com.vividsolutions.jts.geom.Coordinate
import org.scalatest._


abstract class FortuneSpec extends FlatSpec with Matchers


class CenterSpec extends FortuneSpec {

  "computeCenter" should "compute the center of the site of the arc given in parameters and the site of the neighbours arc" in {
    val arc1 = new Arc(new Coordinate(0,10), None, None, None)
    val arc2 = new Arc(new Coordinate(45,4), None, None, None)
    val arc3 = new Arc(new Coordinate(34,20), None, None, None)

    arc2.pred = Some(arc1)
    arc2.next = Some(arc3)


    val center = Fortune.computeCenter(arc2)

    center should be ('defined)
    center should be (Some(new Coordinate(21.51834862385321, -0.36238532110093047)))
  }

  it should "return none if there is no left neighbor or no right neighbor" in {

    val arc2 = new Arc(new Coordinate(45,4), None, None, None)
    val arc3 = new Arc(new Coordinate(34,20), None, None, None)

    arc2.next = Some(arc3)


    val center = Fortune.computeCenter(arc2)
    center should be ('empty)

  }
  // What happens if the right neighbor and the left neighbor correspond to the same site ?
  // Should not happen but who nows ?

}

class BreakPointSpec extends FortuneSpec {
   "breakPoint" should "compute the intersection of the parabola defined by the coordinates given in parameters and the directrice y" in {
     val order = new NodeOrdering(0)
     val sites = (new Coordinate(0,10), new Coordinate(10,1))
     val ix = order.breakPoint(sites)
     ix should be (new Coordinate(6.38398946973719, 2.0377660774857667))

     val order2 = new NodeOrdering(-14)
     val sites2 = (new Coordinate(-34,20), new Coordinate(45,4))
     val ix2 = order2.breakPoint(sites2)
     ix2 should be (new Coordinate(9.247993998090438, 13.505720365571566))




   }

}