package drivesimulator

import java.io.{File, FileNotFoundException, PrintWriter}
import java.nio.file.{Files, Paths}

/**
  * Created by alessandro on 23/02/17.
  */
object Main {

  val usage =
    """
       Usage: drive-simulator --from num --to num --out-dir dirname --out-filename filename
    """.stripMargin

  def main(args: Array[String]): Unit = {

    if (args.length != 8) {
      println(usage)
      System.exit(0)
    }

    var from = ""
    var to = ""
    var dir = ""
    var filename = ""

    args.sliding(2, 1).toList.collect {
      case Array("--from", argFrom: String) => from = argFrom
      case Array("--to", argTo: String) => to = argTo
      case Array("--out-dir", argDir: String) => dir = argDir
      case Array("--out-filename", argFilename: String) => filename = argFilename
    }

    var fromTuple = (0.0, 0.0)
    var toTuple = (0.0, 0.0)

    try {

      val arrFrom = from.split(",")
      if (arrFrom.length != 2)
        throw new IllegalArgumentException (from + " is not a valid lat,lon string")
      fromTuple = (arrFrom(0).toDouble, arrFrom(1).toDouble)

      val arrTo = to.split(",")
      if (arrTo.length != 2)
        throw new IllegalArgumentException (to + " is not a valid lat,lon string")
      toTuple = (arrTo(0).toDouble, arrTo(1).toDouble)

      if (!Files.exists(Paths.get(dir)))
        throw new FileNotFoundException(dir + " does not exist!")

    } catch {
      case e: IllegalArgumentException =>
        System.err.println(e)
        System.exit(1)
      case e: NumberFormatException =>
        System.err.println(e)
        System.exit(1)
      case e: FileNotFoundException =>
        System.err.println(e)
        System.exit(1)
    }

    // Trip generation
    print("Trip generation: ... ")
    val path = new Path(fromTuple, toTuple)
    val driver = new Driver(path)
    println("DONE!")

    // Trip saving
    print("Saving the KML file: ... ")
    val pwkml = new PrintWriter(new File(dir + File.separator + filename + ".kml"))
    pwkml.write(driver.toKML)
    pwkml.close()
    println("DONE!")

    print("Saving the CSV file: ... ")
    val pwcsv = new PrintWriter(new File(dir + File.separator + filename + ".csv"))
    pwcsv.write(driver.toCSV())
    pwcsv.close()
    println("DONE!")

  }

}
