

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
      def isItTheEndOfTheMap(newPosition: Rover): Rover =
        if (newPosition.front.x > theMap.xLength + 1)
          Rover(theMap, Position(1, newPosition.position.y), Front(2, newPosition.front.y))
        else
          if (newPosition.front.y > theMap.yLength + 1)
            Rover(theMap, Position(1, newPosition.position.x), Front(2, newPosition.front.x))
          else
          if (newPosition.front.x < 1)
            Rover(theMap, Position(theMap.xLength, newPosition.position.y), Front(theMap.xLength - 1, newPosition.position.y))
          else {
            if(newPosition.front.y < 1)
              Rover(theMap, Position(theMap.yLength, newPosition.position.x), Front(theMap.yLength - 1, newPosition.position.x))
            else
              newPosition
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


val newRover = Rover(MapSize(7, 7), Position(7, 7), Front(7, 8))
newRover.moveForward