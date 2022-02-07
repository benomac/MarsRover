import Mars.Rover.Tech.Challenge.MarsRover.Rover
import Mars.Rover.Tech.Challenge.{Coordinate, East, NonWrapped, West, Wrapped}

def calculateYMoves(start: Rover, finish: Coordinate, moves: List[String] = Nil): List[String] = {
  val WrapStatus     = wrapStatus(position.y, finish.y, theMap.yLength)
  //      val yAxisWrapStatus     = wrapStatus(position.y, finish.y, theMap.yLength)

  val finishHeading   = (position.y, finish.y, WrapStatus) match {
    case (_, _, Wrapped)  => if (position.y < finish.y) West else East
    case (_, _, NonWrapped) => if (position.y < finish.y) East else West
  }
  val NumberOfMoves = if (start.position.y != finish.y) {
    if (finishHeading != start.heading)
      calculateMoves(start.turnClockWise, finish, moves :+ "turn")
    else
      calculateMoves(start.moveForward, finish, moves :+ "move forward")
  } else moves


  NumberOfMoves

}