package Mars.Rover.Tech.Challenge

sealed trait Map

final case class MapSize(xLength: Int, yLength: Int) extends Map {
  def size: String = s"(X: $xLength, Y: $yLength)"
}

final case class Coordinate(x: Int, y: Int) extends Map {
  def location: String = s"($x, $y)"


}



trait Wrap extends Map
case object Wrapped extends Wrap
case object NonWrapped extends Wrap

trait CompassPoint extends Map {
  def turnClockWise: CompassPoint = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }
  //can these 2 (above and below) be refactored into one method
  def turnAntiClockWise: CompassPoint = this match {
    case North => West
    case East => North
    case South => East
    case West => South
  }
}
case object North extends CompassPoint
case object East extends CompassPoint
case object South extends CompassPoint
case object West extends CompassPoint

final case class Direction(direction: CompassPoint) extends CompassPoint {

}
