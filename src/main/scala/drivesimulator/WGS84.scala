package drivesimulator

import scala.math._

/**
  * Created by Alessandro Adamo on 27/02/17.
  *
  * World Geodetic System 1984 (WGS84)
  */
object WGS84 {

  lazy val A : Double = 6378137.0                 // Semi-major axis a
  lazy val B : Double= 6356752.314245            // Semi-minor axis b
  lazy val IF : Double= 298.257223563            // Inverse flattening (1/f)
  lazy val R : Double = (2.0 * A + B) / 3.0       // Radius for spherical Earth

  lazy val MIN_LATITUDE : Double = -90.0
  lazy val MAX_LATITUDE : Double = +90.0
  lazy val MIN_LONGITUDE : Double  = -180
  lazy val MAX_LONGITUDE : Double = +180

  lazy val GRAVITY = 9.80665

  private def hav(theta: Double) = { val h = sin(0.5 * theta); h * h }

  /**
    * Haversine distance
    *
    * @param pt1 first point
    * @param pt2 second point
    * @return Haversine distance between two points
    */
  def haversine(pt1: (Double, Double), pt2: (Double, Double)) : Double =  {

    val lat1 = pt1._1.toRadians
    val lon1 = pt1._2.toRadians
    val lat2 = pt2._1.toRadians
    val lon2 = pt2._2.toRadians

    val dlat = lat2 - lat1
    val dlong = lon2 - lon1

    2.0 * R * sqrt(hav(dlat) + cos(lat1) * cos(lat2) * hav(dlong))

  }

  /**
    * Given a start point and a distance d along a constant bearing, this will calculate the destination point.
    *
    * @param orig original point
    * @param distance distance expressed in meters
    * @param bearing bearing expressed in degrees
    * @return destination point
    */
  def destination(orig: (Double, Double), distance: Double, bearing: Double): (Double, Double) = {

    val lat1 = orig._1.toRadians
    val lon1 = orig._2.toRadians
    val brng = bearing.toRadians

    val lat2 = asin(sin(lat1) * cos(distance / R) + cos(lat1) * math.sin(distance / R) * cos(brng))
    val lon2 = lon1 + atan2(sin(brng) * sin(distance / R) * cos(lat1),
      cos(distance / R) - sin(lat1) * sin(lat2))
    (lat2.toDegrees, lon2.toDegrees)

  }

  /**
    * Given a start point and a distance d along a constant bearing, this will calculate the destination point.
    *
    * @param orig original point
    * @param vel velocity (meters/seconds)
    * @param time time  (seconds)
    * @param bearing bearing expressed in degrees
    * @return destination point
    */
  def destination(orig: (Double, Double), vel: Double, time: Double, bearing: Double): (Double, Double) = {

    require(time>1e-6)
    require(vel >= 0.0)

    val distance = vel / time
    destination(orig, distance, bearing)

  }

  /**
    * Calculate initial bearing which if followed in a straight line
    * along a great-circle arc will take you from the start point to the end point.
    *
    * @param from start point
    * @param to end point
    * @return bearing
    */
  def bearing(from: (Double, Double), to: (Double, Double)) : Double = {

    val lat1 = from._1.toRadians
    val lon1 = from._2.toRadians
    val lat2 = to._1.toRadians
    val lon2 = to._2.toRadians

    val clat2 = cos(lat2)
    val dlon = lon2 - lon1

    val y = sin(dlon) * clat2
    val x = cos(lat1) * sin(lat2) - sin(lat1) * clat2 * cos(dlon)

    val ang = atan2(y, x).toDegrees
    if (ang < 0.0) 360.0 + ang else ang

  }

  /**
    * Signed Cross track distance
    *
    * @param pt point
    * @param from start point of the path
    * @param to end point of the path
    * @return distance to great circle (-ve if to left, +ve if to right of path)
    * */
  def crossTrackDistance(pt: (Double, Double), from: (Double, Double), to: (Double, Double)) : Double = {

    val delta13 = haversine(from, pt) / R
    val theta13 = bearing(from, pt).toRadians
    val theta12 = bearing(from, to).toRadians

    asin(sin(delta13) * sin(theta13 - theta12)) * R

  }

  /**
    * Convert 3D Cartesian coordinate to latitude/longitude coordinate
    *
    * @param cart 3D cartesian coordinate point
    * @return geodesic converted point
    */
  def cart2geodesic(cart: (Double, Double, Double)) = (asin(cart._3 / R).toDegrees, atan2(cart._2, cart._1).toDegrees)

  /**
    * Convert latitude/longitude coordinate to 3D Cartesian coordinate
    *
    * @param pt geodesic point
    * @return the 3D Cartesian coordinate and value
    */
  def geodesic2cart(pt: (Double, Double)): (Double, Double, Double) = {

    val lat = pt._1.toRadians
    val lon = pt._2.toRadians

    val slat = sin(lat)
    val clat = cos(lat)
    val slong = sin(lon)
    val clong = cos(lon)

    (R * clat * clong, R * clat * slong, R * slat)

  }


  /**
    * Calculate the half-way point along a great circle path between the two points
    *
    * @param pt1 first point
    * @param pt2 second point
    * @return midpoint
    */
  def midpoint(pt1: (Double, Double), pt2: (Double, Double)): (Double, Double) = {

    val pt1c = geodesic2cart(pt1)
    val pt2c = WGS84.geodesic2cart(pt2)

    val m = (0.5 * (pt1c._1 + pt2c._1),
      0.5 * (pt1c._2 + pt2c._2),
      0.5 * (pt1c._3 + pt2c._3))

    WGS84.cart2geodesic(m)

  }

  /**
    * Returns the angle expressed in radiant between this MapPoint and the MapPoint passed as argument.
    * This is the same as the distance on the unit sphere.
    *
    * @param pt1 the first point
    * @param pt2 the second point
    * @return the angle between this point and that point
    */
  def angleBetween(pt1: (Double, Double), pt2: (Double, Double)): Double = {

    val pt1c = geodesic2cart(pt1)
    val pt2c = geodesic2cart(pt2)

    (pt1c._1 * pt2c._1 + pt1c._2 * pt2c._2 + pt1c._3 * pt2c._3) / (R * R)

  }

  /**
    * Returns the LatLng which lies the given fraction of the way between the
    * origin LatLng and the destination LatLng.
    * @param pt1 first point.
    * @param pt2 second point.
    * @param fraction A fraction of the distance to travel.
    * @return The interpolated LatLng.
    */
  def interpolate(pt1: (Double, Double), pt2: (Double, Double), fraction: Double): (Double, Double) = {

    val fromLat = pt1._1.toRadians
    val fromLng = pt1._2.toRadians
    val toLat = pt2._1.toRadians
    val toLng = pt2._2.toRadians
    val cosFromLat = cos(fromLat)
    val cosToLat = cos(toLat)

    // Computes Spherical interpolation coefficients.
    val angle = angleBetween(pt1, pt2)

    val sinAngle = sin(angle)

    val a = sin((1 - fraction) * angle) / sinAngle
    val b = sin(fraction * angle) / sinAngle

    // Converts from polar to vector and interpolate.
    val x = a * cosFromLat * cos(fromLng) + b * cosToLat * cos(toLng)
    val y = a * cosFromLat * sin(fromLng) + b * cosToLat * sin(toLng)
    val z = a * sin(fromLat) + b * sin(toLat)

    (atan2(z, sqrt(x * x + y * y)).toDegrees, atan2(y, x).toDegrees)

  }

  /**
    * Gravity dependant from latitude
    *
    * @param latitude latitude
    * @param altitude atlitude from the sea
    * @return Gravity dependant from latitude
    *
    * */
  def gravityLatitudeModel(latitude: Double, altitude: Double = 0.0) : Double = {
    val lat = latitude.toDegrees
    val c2l = cos(2.0 * lat)
    9.780327 * (1.0026454 - 0.0026512 * c2l + 0.0000058 * c2l * c2l)
  }

}
