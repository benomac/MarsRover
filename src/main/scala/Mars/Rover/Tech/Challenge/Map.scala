package Mars.Rover.Tech.Challenge

import scala.annotation.tailrec

sealed trait Map

final case class MapSize(xLength: Int, yLength: Int) extends Map {

  def size: String = s"(X: $xLength, Y: $yLength)"

  def createListOfCoordinates: List[List[Coordinate]] = {
    val coordinateList: Seq[Coordinate] = for {
      i: Int <- Range(1, this.xLength + 1)
      j: Int <- Range(1, this.yLength + 1)
    } yield Coordinate(i, j)
    createGrid(coordinateList.toList)
  }

  @tailrec
  private def createGrid(coList: List[Coordinate], acc: List[List[Coordinate]] = Nil): List[List[Coordinate]] = {
    if (coList == Nil) acc
    else {
      val l = coList.filter(co => co.y == coList.head.y)
      createGrid(coList.filterNot(co => co.y == coList.head.y), acc :+ l)
    }
  }

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
