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

//    def finishXAxisHeading(finishX: Int, xAxisWrapStatus: Wrap): CompassPoint = (position.x, finishX, xAxisWrapStatus) match {
//      case (_, _, Wrapped)  => if (position.x < finishX) West else East
//      case (_, _, NonWrapped) => if (position.x < finishX) East else West
//    }
//
//    def finishYAxisHeading(finishY: Int, yAxisWrapStatus: Wrap): CompassPoint = (position.y, finishY, yAxisWrapStatus) match {
//      case (_, _, Wrapped)  => if (position.y < finishY) West else East
//      case (_, _, NonWrapped) => if (position.x < finishY) East else West
//    }

    def finishHeading(xOrYPosition: Int, finish: Int, WrapStatus: Wrap, compassPoint1: CompassPoint, compassPoint2: CompassPoint): CompassPoint = (position.x, finish, WrapStatus) match {
      case (_, _, Wrapped)  => if (xOrYPosition < finish) compassPoint1 else compassPoint2
      case (_, _, NonWrapped) => if (xOrYPosition < finish) compassPoint2 else compassPoint1 //make start and finish types?
    }

//    val finishYAxisHeading   = (position.y, finish.y, WrapStatus) match {
//      case (_, _, Wrapped) => if (position.y < finish.y) North else South
//      case (_, _, NonWrapped) => if (position.y < finish.y) South else North

//    def axisNumberOfMoves(finishX: Int, finishHeading: CompassPoint, fin: Coordinate, m: List[String]): List[String] = this match {
//      case start if start.position.x != finishX && finishHeading != start.heading => calculateXMoves(start.turnClockWise, fin, m :+ "turn ")
//    }

    def calculateXMoves(finish: Coordinate, moves: List[String] = Nil): List[String] = {
      val xAxisWrapStatus     = wrapStatus(position.x, finish.x, theMap.xLength)
      val finishXAxisHead      = finishHeading(position.x, finish.x, xAxisWrapStatus, West, East)

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

    def calculateXMoveHeadingAfterMoves(finish: Coordinate, newHeading: CompassPoint = this.heading): CompassPoint = {
      val xAxisWrapStatus     = wrapStatus(position.x, finish.x, theMap.xLength)
      val finishXAxisHead      = finishHeading(position.x, finish.x, xAxisWrapStatus, West, East)
      val Heading =
        if (finishXAxisHead != newHeading)
          calculateXMoveHeadingAfterMoves(finish, newHeading.turnClockWise)
        else newHeading
      Heading
    }

    def calculateYMoves(finish: Coordinate, moves: List[String] = Nil): List[String] = {
      val yAxisWrapStatus     = wrapStatus(position.y, finish.y, theMap.yLength)
      val finishYAxisHead      = finishHeading(position.y, finish.y, yAxisWrapStatus, North, South)

      val yAxisMovesList =
        if (this.position.y != finish.y) {
          if (finishYAxisHead != this.heading) {
            println("y heading " + this.heading)
            this.turnClockWise.calculateYMoves(finish, moves :+ "turn ")
          }
          else
            this.moveForward.calculateYMoves(finish, moves :+ "move forward ")
        } else moves
      yAxisMovesList
    }

    def calculateAllMoves(finish: Coordinate): List[String] = {
      val xMoves = calculateXMoves(finish)
      val yMoves = calculateYMoves(finish)
      List.concat(xMoves, yMoves)
    }



    def currentLocationInfo(newMap: MapSize): String =
      s"Map size: ${newMap.size} Rover's location: $location, Facing: $direction"
  }

}