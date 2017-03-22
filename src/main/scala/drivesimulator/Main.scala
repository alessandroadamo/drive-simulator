package drivesimulator

/**
  * Created by alessandro on 23/02/17.
  */
object Main extends App {

  //def main(args: Array[String]): Unit = {

  var from = (38.339101, 15.864459)
  //val from = (45.499383, 8.926511)
  var to = (38.372488, 15.867626)
  //val to = (45.499876, 8.930690)

  val sim = new Path(from, to)

//  sim.path foreach println

  import java.io._
/*
  val pw = new PrintWriter(new File("/home/user/Desktop/test.kml"))
  pw.write(sim.toKML)
  pw.close()
*/

  val driver = new Driver(sim)

  val trip = new PrintWriter(new File("/home/user/Desktop/trip.kml"))
  trip.write(driver.toKML)
  trip.close()

  driver.trip.foreach(x => println (f"(${x.position._1}%3.6f, ${x.position._2}%3.6f): vel: ${x.velocity * 3.6}%4.2f ele: ${x.altitude}%4.2f "))

  val tripCSV = new PrintWriter(new File("/home/user/Desktop/trip.csv"))
  tripCSV.write(driver.toCSV())
  tripCSV.close()

  println(Driver.curvatureRadius((45.627899, 8.842758), (45.627712, 8.842920), (45.627755, 8.843183)))
  println(Driver.curvatureRadius((45.627779, 8.843363), (45.627741, 8.843457), (45.627700, 8.843572)))

}
