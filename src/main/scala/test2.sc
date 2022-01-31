import MarsRover._

object MarsRover {

  trait Mars

  final case class Rover(theMap: MapSize, position: CoOrdinate, facing: Direction) extends Mars {
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
        case newP if newP.x < 0 => Rover(theMap, CoOrdinate(theMap.xLength, position.y), facing)
        case newP if newP.y > theMap.yLength => Rover(theMap, CoOrdinate(position.x, 1), facing)
        case newP if newP.y < 0 => Rover(theMap, CoOrdinate(position.x, theMap.yLength), facing)
      }

      isItTheEndOfTheMap(newPosition)
    }
  }


  sealed trait Map extends Mars

  final case class MapSize(xLength: Int, yLength: Int) extends Map {
    def size: Map = MapSize(xLength, yLength)
  }
  final case class CoOrdinate(x: Int, y: Int) extends Map

  trait CompassPoint extends Mars
  final case class Direction(direction: CompassPoint = E) extends Map

  final case object N extends CompassPoint
  final case object E extends CompassPoint
  final case object S extends CompassPoint
  final case object W extends CompassPoint

}

import MarsRover._


val newRover = Rover(MapSize(7, 7), CoOrdinate(1, 1), Direction())
newRover.moveForward