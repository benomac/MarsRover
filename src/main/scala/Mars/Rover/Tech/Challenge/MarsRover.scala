package Mars.Rover.Tech.Challenge


object MarsRover {

  final case class Rover(theMap: MapSize, position: CoOrdinate = CoOrdinate(0, 0), heading: Direction = Direction(East)) {
    def location: String = s"${position.location}"
    def direction: String = s"${heading.direction}"

    def moveForward: Rover = {
      val newPosition: CoOrdinate = heading.direction match {
        case North => CoOrdinate(position.x, position.y - 1)
        case East => CoOrdinate(position.x + 1, position.y)
        case South => CoOrdinate(position.x, position.y + 1)
        case West => CoOrdinate(position.x - 1, position.y)
        case _ => position
      }
      def isItTheEndOfTheMap(newPosition: CoOrdinate): Rover = newPosition match {
        case newP if newP.x > theMap.xLength => Rover(theMap, CoOrdinate(1, position.y), heading)
        case newP if newP.y > theMap.yLength => Rover(theMap, CoOrdinate(position.x, 1), heading)
        case newP if newP.x < 0              => Rover(theMap, CoOrdinate(theMap.xLength, position.y), heading)
        case newP if newP.y < 0              => Rover(theMap, CoOrdinate(position.x, theMap.yLength), heading)
        case _ => Rover(theMap, CoOrdinate(newPosition.x, newPosition.y), heading)
      }
      isItTheEndOfTheMap(newPosition)
    }

    def turnClockWise: Rover = heading match {
      case Direction(North) => Rover(theMap, position, Direction(East))
      case Direction(East) => Rover(theMap, position, Direction(South))
      case Direction(South) => Rover(theMap, position, Direction(West))
      case Direction(West) => Rover(theMap, position, Direction(North))
    }

    def turnAntiClockWise: Rover = heading match {
      case Direction(North) => Rover(theMap, position, Direction(West))
      case Direction(East) => Rover(theMap, position, Direction(North))
      case Direction(South) => Rover(theMap, position, Direction(East))
      case Direction(West) => Rover(theMap, position, Direction(South))
    }

    def getFromAToB(a: CoOrdinate, b: CoOrdinate, moves: List[String] = Nil): String = {
      val movesForward = b.x - a.x
      val movesDown = b.y - a.y
      val floor = movesForward + movesDown

      if (a.y == b.y) (b.x - a.x).toString
      else
        if (a.x != b.x) (b.y - a.y).toString

      s"$floor moves to location ${b.location}"

    }

    def currentLocationInfo(newMap: MapSize): String =
      s"Map size: ${newMap.size} Rover's location: $location, Facing: $direction"
  }

}