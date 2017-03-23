package drivesimulator

import scala.math._

case class Sample(position: (Double, Double),
                  bearing: Double,
                  velocity: Double,
                  deltaDistance: Double,
                  altitude: Double,
                  slope: Double,
                  acceleration: (Double, Double, Double) = (0.0, 0.0, 0.0))

object Driver {

  lazy val DISTANCE_TOLL = 1e-1
  lazy val CHUNK_SIZE = 100
  lazy val EPS = 1e-9

  /**
    * Compute the Menger radius of curvature between three points
    *
    * @param pt1 first point
    * @param pt2 second point
    * @param pt3 third point
    * @return radius of curvature
    **/
  def curvatureRadius(pt1: (Double, Double), pt2: (Double, Double), pt3: (Double, Double)): Double = {

    val p1 = WGS84.geodesic2cart(pt1)
    val p2 = WGS84.geodesic2cart(pt2)
    val p3 = WGS84.geodesic2cart(pt3)

    val v1 = (p1._1 - p2._1, p1._2 - p2._2, p1._3 - p2._3)
    val v1mag = sqrt(v1._1 * v1._1 + v1._2 * v1._2 + v1._3 * v1._3)
    val v1n = (v1._1 / v1mag, v1._2 / v1mag, v1._3 / v1mag)

    val v2 = (p3._1 - p2._1, p3._2 - p2._2, p3._3 - p2._3)
    val v2mag = sqrt(v2._1 * v2._1 + v2._2 * v2._2 + v2._3 * v2._3)
    val v2n = (v2._1 / v2mag, v2._2 / v2mag, v2._3 / v2mag)

    val res = v1n._1 * v2n._1 + v1n._2 * v2n._2 + v1n._3 * v2n._3
    val alpha = acos(res)

    val d = sqrt(pow(p1._1 - p3._1, 2.0) + pow(p1._2 - p3._2, 2.0) + pow(p1._3 - p3._3, 2.0))
    val k = (2.0 * sin(alpha)) / (d + Driver.EPS) // curvature

    rotationDirection(pt1, pt2, pt3) / (k + Driver.EPS)

  }


  /**
    * Roration direction of a triangle
    *
    * @param pt1 First point
    * @param pt2 Second point
    * @param pt3 Third point
    * @return Sign of rotation. The value is -1.0 if the rotation direction is left, 1.0 if is wright and 0.0 otherwhise
    *
    * */
  def rotationDirection(pt1: (Double, Double), pt2: (Double, Double), pt3: (Double, Double)): Double =
    - signum((pt2._1 - pt1._1) * (pt3._2 - pt1._2) - (pt3._1 - pt1._1) * (pt2._2 - pt1._2))

}

/**
  * Created by Alessandro Adamo on 27/02/17.
  *
  * @param path Path object
  * @param fs   sampling frequency (default 1Hz)
  */
class Driver(path: Path, fs: Double = 1.0) {

  require(path != null)
  require(path.path.nonEmpty)
  require(fs > 0.0)

  val dt: Double = 1.0 / fs

  val trip: List[Sample] = generateSamples()

  /**
    * Generate samples the next sample
    *
    **/
  private def generateSamples(): List[Sample] = {

    var positions = scala.collection.mutable.ListBuffer[(Double, Double)]()

    // for each path step
    for (ps <- path.path) {

      val nsamples = math.ceil(ps.deltaTime / dt).toInt
      val dw = 1.0 / nsamples

      for (it <- 0 until nsamples) {
        positions += WGS84.interpolate(ps.from, ps.to, dw * it)
      }

    }

    // calculate positions
    positions += positions.tail(0)
    val positionsList = positions.sliding(2).map(x => Sample(position = x(0),
      bearing = WGS84.bearing(x(0), x(1)),
      velocity = WGS84.haversine(x(0), x(1)) / dt,
      deltaDistance = WGS84.haversine(x(0), x(1)),
      altitude = 0.0,
      slope = 0.0))
      .toList
      .dropRight(1)

    // Get elevations: partitioning the set
    val chunks = positionsList.grouped(Driver.CHUNK_SIZE)
    var rests = scala.collection.mutable.ListBuffer[Sample]()

    // for each chunk
    for (chunk <- chunks) {

      var str = ""
      for (p <- chunk) {
        str += p.position._1 + "," + p.position._2 + "|"
      }
      str = str.substring(0, str.length - 1)
      str = str.substring(0, str.length - 1)

      // Elevation
      val urlElevation = "https://maps.googleapis.com/maps/api/elevation/xml?" +
        "locations=" + str +
        "&key=" + Path.APP_KEY

      val xmlElevation = scala.xml.XML.loadString(
        scala.io.Source.fromURL(urlElevation).mkString
      )

      if ((xmlElevation \ "status").text == "OK") {

        val lat = xmlElevation \\ "result" \\ "location" \\ "lat"
        val lon = xmlElevation \\ "result" \\ "location" \\ "lng"
        val el = xmlElevation \\ "result" \\ "elevation"

        val app = for (i <- (0 until lat.length).toList)
          yield Sample(position = (lat(i).text.toDouble, lon(i).text.toDouble),
            altitude = el(i).text.toDouble,
            bearing = chunk(i).bearing,
            deltaDistance = chunk(i).deltaDistance,
            velocity = chunk(i).velocity,
            slope = 0.0)

        rests ++= app

      }

    }

    // calculate the slope between samples
    rests += rests.tail(0)

    val data = rests.sliding(2).map(x => Sample(position = x.head.position,
      bearing = x(0).bearing,
      velocity = x(0).velocity,
      deltaDistance = x(0).deltaDistance,
      altitude = x(0).altitude,
      slope = (x(1).altitude - x(0).altitude) / x(0).deltaDistance))
      .toList
      .dropRight(1)

    val ax = (data :+ data.tail(0)).sliding(2)
      .map(x => (x(0).velocity - x(1).velocity) / dt)
      .toArray

    val ay = ((data.head :: data) :+ data.tail(0)).sliding(3)
      .map(x => pow(x(1).velocity, 2.0) / Driver.curvatureRadius(x(0).position, x(1).position, x(2).position))
      .toArray

    val az = data.map(x => WGS84.gravityLatitudeModel(latitude = x.position._1, altitude = x.altitude))
      .toArray

    val app = data.toArray

    for (i <- (0 until app.length).toList)
      yield Sample(position = app(i).position,
        bearing = app(i).bearing,
        velocity = app(i).velocity,
        deltaDistance = app(i).deltaDistance,
        altitude = app(i).altitude,
        slope = app(i).slope,
        acceleration = (ax(i), ay(i), az(i)))

  }


  /**
    * Convert the simulated trip to KML
    *
    * @return KML of the trip
    **/
  def toKML: String =
    <kml xmlns="http://earth.google.com/kml/2.0">
      <Document>
        {trip.map(x =>
        <Placemark>
          <ExtendedData>
            <Data name="bearing">
              <value>
                {x.bearing}
              </value>
            </Data>
            <Data name="slope">
              <value>
                {x.slope}
              </value>
            </Data>
            <Data name="deltaDistance">
              <value>
                {x.deltaDistance}
              </value>
            </Data>
            <Data name="velocity">
              <value>
                {x.velocity}
              </value>
            </Data>
            <Data name="altitude">
              <value>
                {x.altitude}
              </value>
            </Data>
            <Data name="ax">
              <value>
                {x.acceleration._1}
              </value>
            </Data>
            <Data name="ay">
              <value>
                {x.acceleration._2}
              </value>
            </Data>
            <Data name="az">
              <value>
                {x.acceleration._3}
              </value>
            </Data>
          </ExtendedData>
          <Point>
            <coordinates>
              {x.position._2}
              ,
              {x.position._1}
              ,
              {x.altitude}
            </coordinates>
          </Point>
        </Placemark>
      )}
      </Document>
    </kml>.toString


  def toCSV(sep: String = ";"): String = {
    val buffer = new StringBuilder
    // write header
    buffer ++= "latitude" + sep +
      "longitude" + sep +
      "bearing(deg)" + sep +
      "velocity(m/s)" + sep +
      "deltaDistance(m)" + sep +
      "altitude(m)" + sep +
      "slope(rad)" + sep +
      "ax(m/s^2)" + sep +
      "ay(m/s^2)" + sep +
      "az(m/s^2)" + sep + "\n"
    // write records
    trip foreach (x => buffer ++= x.position._1 + sep + // latitude
      x.position._2 + sep + // longitude
      x.bearing + sep + // bearing
      x.velocity + sep + // velocity
      x.deltaDistance + sep + // deltaDistance
      x.altitude + sep + // altitude
      x.slope + sep + // slope
      x.acceleration._1 + sep +
      x.acceleration._2 + sep +
      x.acceleration._3 + sep + "\n") // accelerations
    buffer.toString
  }

}
