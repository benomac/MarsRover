object MarsRover {

  trait Mars

  final case class Rover(theMap: MapSize, position: CoOrdinate = CoOrdinate(0, 0), facing: Direction) extends Mars {
    def moveForward: Rover = {
      val newPosition: CoOrdinate = facing.direction match {
      case N => CoOrdinate(position.x, position.y - 1)
      case E => CoOrdinate(position.x + 1, position.y)
      case S => CoOrdinate(position.x, position.y + 1)
      case W => CoOrdinate(position.x - 1, position.y)
      case _ => position
    }
      def isItTheEndOfTheMap(newPosition: CoOrdinate): Rover = newPosition match {
        case newP if newP.x > theMap.xLength => Rover(theMap, CoOrdinate(1, position.y), facing)
        case newP if newP.y > theMap.yLength => Rover(theMap, CoOrdinate(position.x, 1), facing)
        case newP if newP.x < 0              => Rover(theMap, CoOrdinate(theMap.xLength, position.y), facing)
        case newP if newP.y < 0              => Rover(theMap, CoOrdinate(position.x, theMap.yLength), facing)
        case _ => Rover(theMap, CoOrdinate(newPosition.x, newPosition.y), facing)
      }
      isItTheEndOfTheMap(newPosition)
    }

    def turnRight: Rover = facing match {
      case Direction(N) => Rover(theMap, position, Direction(E))
      case Direction(E) => Rover(theMap, position, Direction(S))
      case Direction(S) => Rover(theMap, position, Direction(W))
      case Direction(W) => Rover(theMap, position, Direction(N))
    }

    def turnLeft: Rover = facing match {
      case Direction(N) => Rover(theMap, position, Direction(W))
      case Direction(E) => Rover(theMap, position, Direction(N))
      case Direction(S) => Rover(theMap, position, Direction(E))
      case Direction(W) => Rover(theMap, position, Direction(S))
    }
  }



  sealed trait Map extends Mars

  final case class MapSize(xLength: Int, yLength: Int) extends Map {
    def size: Map = MapSize(xLength, yLength)
  }

  final case class CoOrdinate(x: Int, y: Int) extends Map

  trait CompassPoint extends Mars
  final case object N extends CompassPoint
  final case object E extends CompassPoint
  final case object S extends CompassPoint
  final case object W extends CompassPoint

  final case class Direction(direction: CompassPoint = E) extends Map

}

import MarsRover._


val newRover = Rover(MapSize(5, 6), CoOrdinate(0, 0), Direction())
println(9)
newRover
newRover.moveForward.turnLeft.moveForward
newRover.moveForward