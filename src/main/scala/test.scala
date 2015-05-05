import com.vividsolutions.jts.geom.GeometryFactory

/**
 * Created by Fabian on 26-04-15.
 */
object Test {
  def run(algo : String, n : Array[Int], x : Int): Unit ={
    val fact = new GeometryFactory()
    val result = new Array[Long](n.length)
    algo match{
      case "Naive" => {
        for(i <- 0 until n.length){
          val tmpRes = new Array[Long](x)
          for(j <- 0 until x){
            val naive = new Naive(GenPoints.generate(1000, 1000, n(i)))
            val start = System.currentTimeMillis()
            val useless = naive.run()
            tmpRes(j) = System.currentTimeMillis() - start
          }
          result(i) = tmpRes.reduceLeft(_+_)/x
          println(result(i))
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    run("Naive", Array(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600), 5)
  }
}
