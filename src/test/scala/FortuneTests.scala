/**
 * Created by ludov on 30/04/15.
 */

import com.vividsolutions.jts.geom.Coordinate
import org.scalatest._


abstract class FortuneSpec extends FlatSpec with Matchers


class CenterSpec extends FortuneSpec {

  "computeCenter" should "compute the center of the site of the arc given in parameters and the site of the neighbours arc" in {

  }

  it should "return none if there is no left neighbor or no right neighbor" in {
    
  }

}

class BreakPointSpec extends FortuneSpec {
   "breakPoint" should "compute the intersection of the parabola defined by the coordinates given in parameters and the directrice y" in {
     val order = new NodeOrdering(0)
     val sites = (new Coordinate(0,10), new Coordinate(10,1))
     val ix = order.breakPoint(sites)
     ix should be () //???
   }

}