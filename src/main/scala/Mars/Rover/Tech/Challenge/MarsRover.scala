package Mars.Rover.Tech.Challenge


object MarsRover {

  final case class Rover(theMap: TheMapSize, position: Coordinate, heading: CompassPoint) {
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

    def wrapStatus(axisB: Int, axisA: Int, mapSide: Int): Wrap = {
      val AToB = Math.abs(axisB - axisA)
      val lengthLessAToB = mapSide - AToB
      if (AToB > lengthLessAToB) Wrapped else NonWrapped
    }

    def finishHeading(xOrYPosition: Int, finish: Int, WrapStatus: Wrap, compassPoint1: CompassPoint, compassPoint2: CompassPoint): CompassPoint = (position.x, finish, WrapStatus) match {
      case (_, _, Wrapped)  => if (xOrYPosition > finish) compassPoint1 else compassPoint2
      case (_, _, NonWrapped) => if (xOrYPosition < finish) compassPoint1 else compassPoint2 //make start and finish types?
    }

    def calculateXMoves(finish: Coordinate, moves: List[String] = Nil): List[String] = {
      val xAxisWrapStatus     = wrapStatus(position.x, finish.x, theMap.xLength)
      val finishXAxisHead      = finishHeading(position.x, finish.x, xAxisWrapStatus, East, West)

      val xAxisMovesList =
        if (this.position.x != finish.x) {
        if (finishXAxisHead != this.heading) {
          println("x heading " + this.heading)
          this.turnClockWise.calculateXMoves(finish, moves :+ "turn ")
        }
          else
          this.moveForward.calculateXMoves(finish, moves :+ "move forward ")
      } else moves
      xAxisMovesList
    }

    def calculateHeadingAfterXmoves(finish: Coordinate, newHeading: CompassPoint = this.heading): CompassPoint = {
      val xAxisWrapStatus     = wrapStatus(position.x, finish.x, theMap.xLength)
      val finishXAxisHead      = finishHeading(position.x, finish.x, xAxisWrapStatus, East, West)
      val Heading =
        if (finishXAxisHead != newHeading)
          calculateHeadingAfterXmoves(finish, newHeading.turnClockWise)
        else newHeading
      Heading
    }

    def calculateYMoves(finish: Coordinate, moves: List[String] = Nil, newH: CompassPoint): List[String] = {
      val yAxisWrapStatus     = wrapStatus(position.y, finish.y, theMap.yLength)
      val finishYAxisHead      = finishHeading(position.y, finish.y, yAxisWrapStatus, South, North)
      val newRover = Rover(theMap, position, newH)
      val yAxisMovesList =
        if (newRover.position.y != finish.y) {
          if (finishYAxisHead != newRover.heading) {
            println("x heading " + newRover.heading)
            calculateYMoves(finish, moves :+ "turn ", newH.turnClockWise)
          }
          else
            newRover.moveForward.calculateYMoves(finish, moves :+ "move forward ", newH)
        } else moves
      yAxisMovesList

    }

    def calculateAllMoves(finish: Coordinate): List[String] = {
      val xMoves = calculateXMoves(finish)
      val newH = calculateHeadingAfterXmoves(finish, heading)
      val yMoves = calculateYMoves(finish, newH = newH)
      List.concat(xMoves, yMoves)
    }



    def currentLocationInfo(newMap: TheMapSize): String =
      s"TheMap size: ${newMap.size} Rover's location: $location, Facing: $direction"
  }

}