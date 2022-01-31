package Mars.Rover.Tech.Challenge

sealed trait Map

final case class MapSize(xLength: Int, yLength: Int) extends Map {
  def size: String = s"(X: $xLength, Y: $yLength)"
}

final case class CoOrdinate(x: Int, y: Int) extends Map {
  def location: String = s"($x, $y)"}

trait CompassPoint extends Map
case object North extends CompassPoint
case object East extends CompassPoint
case object South extends CompassPoint
case object West extends CompassPoint

final case class Direction(direction: CompassPoint = East) extends CompassPoint
