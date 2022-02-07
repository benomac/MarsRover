package Mars.Rover.Tech.Challenge


object MarsRover {

  final case class Rover(theMap: MapSize, position: Coordinate, heading: CompassPoint) {
    def location: String = s"${position.location}"
    def direction: String = s"${heading}"

    def moveForward: Rover = {
      val newPosition: Coordinate = heading match {
        case North => Coordinate(position.x, position.y - 1)
        case East => Coordinate(position.x + 1, position.y)
        case South => Coordinate(position.x, position.y + 1)
        case West => Coordinate(position.x - 1, position.y)
        case _ => position
      }
      def wrap(newPosition: Coordinate): Rover = newPosition match {
        case newP if newP.x > theMap.xLength => Rover(theMap, Coordinate(1, position.y), heading)
        case newP if newP.y > theMap.yLength => Rover(theMap, Coordinate(position.x, 1), heading)
        case newP if newP.x < 1              => Rover(theMap, Coordinate(theMap.xLength, position.y), heading)
        case newP if newP.y < 1              => Rover(theMap, Coordinate(position.x, theMap.yLength), heading)
        case _ => Rover(theMap, Coordinate(newPosition.x, newPosition.y), heading)
      }
      wrap(newPosition)
    }

    def turnClockWise: Rover = heading match {
      case North => Rover(theMap, position, East)
      case East => Rover(theMap, position, South)
      case South => Rover(theMap, position, West)
      case West => Rover(theMap, position, North)
    }
    //can these 2 (above and below) be refactored into one method
    def turnAntiClockWise: Rover = heading match {
      case North => Rover(theMap, position, West)
      case East => Rover(theMap, position, North)
      case South => Rover(theMap, position, East)
      case West => Rover(theMap, position, South)
    }

//    private def getNumberOfMovesOnOneAxis(axisB: Int, axisA: Int, mapSide: Int): Int = {
//      val AToB = Math.abs(axisB - axisA)
//      val lengthLessAToB = mapSide - AToB //for the wrap around
//      if (AToB > lengthLessAToB) lengthLessAToB else AToB
//    }

    private def wrapStatus(axisB: Int, axisA: Int, mapSide: Int): Wrap = {
      val AToB = Math.abs(axisB - axisA)
      val lengthLessAToB = mapSide - AToB
      if (AToB > lengthLessAToB) Wrapped else NonWrapped
    }


    def calculateXMoves(start: Rover, finish: Coordinate, moves: List[String] = Nil): List[String] = {
      val xAxisWrapStatus     = wrapStatus(position.x, finish.x, theMap.xLength)

      val finishXAxisHeading   = (position.x, finish.x, xAxisWrapStatus) match {
        case (_, _, Wrapped)  => if (position.x < finish.x) West else East
        case (_, _, NonWrapped) => if (position.x < finish.x) East else West
      }

      val xAxisNumberOfMoves = if (start.position.x != finish.x) {
        if (finishXAxisHeading != start.heading)
          calculateXMoves(start.turnClockWise, finish, moves :+ "turn ")
          else
          calculateXMoves(start.moveForward, finish, moves :+ "move forward ")
      } else moves
      xAxisNumberOfMoves
    }

    def calculateYMoves(start: Rover, finish: Coordinate, moves: List[String] = Nil): List[String] = {
      val WrapStatus     = wrapStatus(position.y, finish.y, theMap.yLength)

      val finishHeading   = (position.y, finish.y, WrapStatus) match {
        case (_, _, Wrapped) => if (position.y < finish.y) North else South
        case (_, _, NonWrapped) => if (position.y < finish.y) South else North
      }
      val NumberOfMoves = if (start.position.y != finish.y) {
        if (finishHeading != start.heading)
          calculateYMoves(start.turnClockWise, finish, moves :+ "turn ")
        else
          calculateYMoves(start.moveForward, finish, moves :+ "move forward ")
      } else moves
      NumberOfMoves
    }



    def currentLocationInfo(newMap: MapSize): String =
      s"Map size: ${newMap.size} Rover's location: $location, Facing: $direction"
  }

}