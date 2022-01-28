

object MarsRover {

  trait Mars

  final case class Rover(position: Position, front: Front) extends Mars{
    def moveForward: Rover = {
      val newFront: Front = (position, front) match {
      case (p, f) if f.y == p.y + 1 && f.x == p.x => Front(f.x, f.y + 1)
      case (p, f) if f.x == p.x - 1 && f.y == p.y => Front(f.x - 1, f.y)
      case (p, f) if f.x == p.x + 1 && f.y == p.y => Front(f.x + 1, f.y)
      case (p, f) if f.y == p.y - 1 && f.x == p.x => Front(f.x, f.y - 1)
      case _ => front
    }
      // Stops the Rover from moving if the front is an incorrect coOdinate
      if (newFront == front)
        Rover(position, front)
      else
        Rover(Position(front.x, front.y), newFront)
    }
  }


  sealed trait CoOrdinate extends Mars
  final case class Position(x: Int, y: Int) extends CoOrdinate
  final case class Front(x: Int, y: Int) extends CoOrdinate

  final case class Map(x: Int, y: Int) extends Mars {
    def size: Map = Map(x, y)
  }

}
import MarsRover._

val f = Map(7, 7)
f.size
val newRover = Rover(Position(2, 2), Front(3, 2))
newRover.moveForward