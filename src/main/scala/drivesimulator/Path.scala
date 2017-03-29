package drivesimulator

object Path {

  lazy val CHUNK_SIZE = 250
  lazy val WINDOW_SIZE = 5

}

case class PathStep(from: (Double, Double),
                    to: (Double, Double),
                    elevation: Double,
                    velocity: Double,
                    deltaDistance: Double, // variational
                    deltaTime: Double, // variational
                    deltaElevation: Double) {
  // variational

  override def toString: String = "(" + from._1 + ", " +
    from._2 + "), " +
    "(" + to._1 + ", " +
    to._2 + "), " +
    elevation + ", " +
    velocity + ", " +
    deltaDistance + ", " +
    deltaTime + ", "
}

/**
  * Created by Alessandro Adamo on 23/02/17.
  *
  * @param from Source point tuple that is expressed in longitude, latitude
  * @param to   Source point tuple that is expressed in longitude, latitude
  */
class Path(from: (Double, Double), to: (Double, Double)) {

  require(from._1 >= WGS84.MIN_LATITUDE && from._1 <= WGS84.MAX_LATITUDE)
  require(from._2 >= WGS84.MIN_LONGITUDE && from._1 <= WGS84.MAX_LONGITUDE)
  require(to._1 >= WGS84.MIN_LATITUDE && to._1 <= WGS84.MAX_LATITUDE)
  require(to._2 >= WGS84.MIN_LONGITUDE && to._1 <= WGS84.MAX_LONGITUDE)

  val path: List[PathStep] = getPath

  /**
    * Decode polyline
    *
    * @param encoded Encoded string
    * @return List of decoded polyline. Each element is a tuple of latitude and longitude coordinates
    **/
  private def decodePolyline(encoded: String): List[(Double, Double)] = {

    if (encoded == null) {
      return List()
    }

    var poly = List[(Double, Double)]()
    var index = 0
    val len = encoded.length
    var lat = 0
    var lng = 0

    while (index < len) {
      var b = 0x20
      var shift = 0
      var result = 0

      do {
        b = encoded.charAt(index) - 63
        index += 1
        result = result | ((b & 0x1f) << shift)
        shift += 5
      } while (b >= 0x20)

      var dlat = 0
      if ((result & 1) != 0)
        dlat = ~(result >> 1)
      else dlat = result >> 1
      lat += dlat

      shift = 0
      result = 0

      do {
        b = encoded.charAt(index) - 63
        index += 1
        result = result | ((b & 0x1f) << shift)
        shift += 5
      } while (b >= 0x20)

      var dlng = 0
      if ((result & 1) != 0)
        dlng = ~(result >> 1)
      else dlng = result >> 1
      lng += dlng

      val p = (lat / 1e5, lng / 1e5)

      poly = poly :+ p

    }

    poly

  }

  /**
    * Get path
    *
    * @return List of tupla that are the latitude, longitude, altitude and resolution of each point
    *         (lat, lon, lat_next, lon_next, elevation, resolution, distance, duration, velocity)
    *
    **/
  private def getPath: List[PathStep] = {

    val orig = from._1.toString + "," + from._2.toString
    val dest = to._1.toString + "," + to._2.toString
    // url of the rest service
    val url = "https://maps.googleapis.com/maps/api/directions/xml?" +
      "origin=" + orig +
      "&destination=" + dest +
      "&key=" + AppKey.APP_KEY

    val xml = scala.xml.XML.loadString(
      scala.io.Source.fromURL(url).mkString
    )

    // steps
    // (start_lat, start_lon, end_lat, end_lon, distance, duration, velocity, polyline)
    val steps = xml \\ "route" \\ "leg" \\ "step"
    var stps = List[(Double, Double, Double, Double, Double, Double, Double, String)]()
    for (st <- steps)
      stps = stps :+ (
        (st \ "start_location" \ "lat").text.toDouble, (st \ "start_location" \ "lng").text.toDouble,
        (st \ "end_location" \ "lat").text.toDouble, (st \ "end_location" \ "lng").text.toDouble,
        (st \ "distance" \ "value").text.toDouble,
        (st \ "duration" \ "value").text.toDouble,
        (st \ "distance" \ "value").text.toDouble / (st \ "duration" \ "value").text.toDouble,
        (st \ "polyline" \ "points").text
      )

    try {
      if (steps.size <= 1)
        throw new IllegalArgumentException("Too few steps founded!")
    } catch {
      case e: IllegalArgumentException =>
        System.err.println(e)
        System.exit(1)
    }

    // explode the polyline
    val path = stps.flatMap(x => decodePolyline(x._8)
      .map(y => (y._1, // polyline_pt_lat
        y._2, // polyline_pt_lon
        x._5, // distance
        x._6, // duration
        x._7 // velocity
      )))

    val chunks = path.grouped(Path.CHUNK_SIZE)
    var rests = List[(Double, Double, Double, Double, Double, Double, Double)]()

    // for each chunk
    for (chunk <- chunks) {

      var str = ""
      for (p <- chunk) {
        str += p._1 + "," + p._2 + "|"
      }
      str = str.substring(0, str.length - 1)
      str = str.substring(0, str.length - 1)

      // Elevation
      val urlElevation = "https://maps.googleapis.com/maps/api/elevation/xml?" +
        "locations=" + str +
        "&key=" + AppKey.APP_KEY

      val xmlElevation = scala.xml.XML.loadString(
        scala.io.Source.fromURL(urlElevation).mkString
      )

      if ((xmlElevation \ "status").text == "OK") {

        val lat = xmlElevation \\ "result" \\ "location" \\ "lat"
        val lon = xmlElevation \\ "result" \\ "location" \\ "lng"
        val el = xmlElevation \\ "result" \\ "elevation"
        val res = xmlElevation \\ "result" \\ "resolution"

        val app = for (i <- (0 until lat.length).toList)
          yield (lat(i).text.toDouble, // latitude
            lon(i).text.toDouble, // longitude
            el(i).text.toDouble, // elevation
            res(i).text.toDouble, // resolution
            chunk(i)._3, // distance
            chunk(i)._4, // duration
            chunk(i)._5) // speed

        rests = rests ::: app

      }

    }

    // Delta distance
    val deltaDists = (rests :+ rests.last).map(x => (x._1, x._2))
      .sliding(2)
      .map(x => WGS84.haversine((x.head._1, x.head._2), (x.last._1, x.last._2)))
      .toList

    val positions = (rests :+ rests.last).map(x => (x._1, x._2))
      .sliding(2)
      .map(x => (x.head._1, x.head._2, x.last._1, x.last._2))
      .toList

    val elevations = rests.map(x => x._3)

    val deltaElevations = (rests :+ rests.last)
      .sliding(2)
      .map(x => x.head._3 - x.last._3)
      .toList

    // Padding
    for (i: Int <- 0 until Path.WINDOW_SIZE - 1)
      rests = rests :+ rests.last

    val vels = rests.map(x => x._7)
      .sliding(Path.WINDOW_SIZE)
      .map(x => x.sum / x.length)
      .toList

    val deltaTimes = deltaDists.zip(vels)
      .map(x => x._1 / x._2)

    // List(positions, elevations, vels, deltaDists, deltaTimes, deltaElevations, deltaVels)
    (0 until positions.length).map { i =>
      PathStep(
        from = (positions(i)._1, positions(i)._2),
        to = (positions(i)._3, positions(i)._4),
        elevation = elevations(i),
        velocity = vels(i),
        deltaDistance = deltaDists(i),
        deltaTime = deltaTimes(i),
        deltaElevation = deltaElevations(i))
    }.toList

  }

  def toKML: String =
    <kml xmlns="http://earth.google.com/kml/2.0">
      <Document>
        {path.map(x =>
        <Placemark>
          <ExtendedData>
            <Data name="elevation">
              <value>
                {x.elevation}
              </value>
            </Data>
            <Data name="velocity">
              <value>
                {x.velocity}
              </value>
            </Data>
            <Data name="deltaDistance">
              <value>
                {x.deltaDistance}
              </value>
            </Data>
            <Data name="deltaDuration">
              <value>
                {x.deltaTime}
              </value>
            </Data>
            <Data name="deltaElevation">
              <value>
                {x.deltaElevation}
              </value>
            </Data>
          </ExtendedData>
          <Point>
            <coordinates>
              {x.from._2}
              ,
              {x.from._1}
              ,
              {x.elevation}
            </coordinates>
          </Point>
        </Placemark>
      )}
      </Document>
    </kml>.toString

}