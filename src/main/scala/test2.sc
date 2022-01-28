

object MarsRover {

  trait Mars

  final case class Rover(theMap: MapSize, position: Position, front: Front) extends Mars{
    def moveForward: Rover = {
      val newFront: Front = (position, front) match {
      case (p, f) if f.y == p.y + 1 && f.x == p.x => Front(f.x, f.y + 1)
      case (p, f) if f.x == p.x - 1 && f.y == p.y => Front(f.x - 1, f.y)
      case (p, f) if f.x == p.x + 1 && f.y == p.y => Front(f.x + 1, f.y)
      case (p, f) if f.y == p.y - 1 && f.x == p.x => Front(f.x, f.y - 1)
      case _ => front
    }
      def isItTheEndOfTheMap(newPosition: Rover): Rover = (newPosition.position, newPosition.front)  match {
        case (position, front) if position.x > theMap.xLength + 1 => Rover(theMap, Position(1, position.y), Front(2, front.y))
        case (position, front) if front.y > theMap.yLength + 1    => Rover(theMap, Position(1, position.x), Front(2, front.x))
        case (position, front) if front.x < 1                     => Rover(theMap, Position(theMap.xLength, position.y), Front(theMap.xLength - 1, position.y))
        case (position, front) if front.y < 1                     => Rover(theMap, Position(theMap.yLength, position.x), Front(theMap.yLength - 1, position.x))
        case  _                                                   => newPosition
      }
          

      // Stops the Rover from moving if the front is an impossible(diagonal) coOrdinate
      val newRoverPostion: Rover = if (newFront == front)
        Rover(theMap, position, front)
      else
        Rover(theMap, Position(front.x, front.y), newFront)

      isItTheEndOfTheMap(newRoverPostion)
    }
  }


  sealed trait Map extends Mars

  final case class MapSize(xLength: Int, yLength: Int) extends Map {
    def size: Map = MapSize(xLength, yLength)
  }
  final case class Position(x: Int, y: Int) extends Map
  final case class Front(x: Int, y: Int) extends Map

}

import MarsRover._


val newRover = Rover(MapSize(7, 7), Position(1, 1), Front(0, 1))
newRover.moveForward