import java.awt.Toolkit

import com.vividsolutions.jts.geom.GeometryFactory

/**
 * AUTHORS :
 * Fabian Souris
 * Ludovic Vanoorenberghe
 */
object Test {
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val winWidth = (screenSize.getWidth-10).toInt
  val winHeight = (screenSize.getHeight-150).toInt


  def run(algo : String, n : Array[Int], x : Int): Unit ={
    val fact = new GeometryFactory()
    val result = new Array[Long](n.length)
    algo match{
      case "Naive" =>
        for (i <- 0 until n.length) {
          val tmpRes = new Array[Long](x)
          for (j <- 0 until x) {
            val naive = new Naive(GenPoints.generate(1000, 1000, n(i)))
            val start = System.currentTimeMillis()
            naive.run
            tmpRes(j) = System.currentTimeMillis() - start
          }
          result(i) = tmpRes.sum / x
          println(n(i) + "\t" + result(i))
        }
      case "Fortune" =>
        for (i <- 0 until n.length) {
          val tmpRes = new Array[Long](x)
          for (j <- 0 until x) {
            val fortune = new Fortune(GenPoints.generate(1000, 1000, n(i)), winWidth, winHeight)
            val start = System.currentTimeMillis()
            fortune.run
            tmpRes(j) = System.currentTimeMillis() - start
          }
          result(i) = tmpRes.sum / x
          println(n(i) + "\t" + result(i))
        }
    }
  }

  def main(args: Array[String]): Unit = {
    run("Naive", Array(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150), 10)
    println("------------------------")
    run("Fortune", Array(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150), 10)
  }
}
