object Movements {

  trait Mars

  final case class Rover(position: CoOrdinate, front: CoOrdinate) extends Mars {

    def moveForwards: Rover = {
      val frontCoX = front.coOrdinate._1
      val frontCoY = front.coOrdinate._2
      val posCoX   = position.coOrdinate._1
      val posCoY   = position.coOrdinate._2

      val newFront: CoOrdinate = (frontCoX, frontCoY, posCoX, posCoY) match {
        case (_, _, _, _) if frontCoX == posCoX + 1 && frontCoY == posCoY => front.coOrdinate(frontCoX + 1, frontCoY)
        case (_, _, _, _) if frontCoY == posCoY + 1 && frontCoX == posCoX => front.coOrdinate(frontCoX, frontCoY + 1)
        case (_, _, _, _) if frontCoX == posCoX - 1 && frontCoY == posCoY => front.coOrdinate(frontCoX - 1, frontCoY)
        case (_, _, _, _) if frontCoY == posCoY - 1 && frontCoX == posCoX => front.coOrdinate(frontCoX, frontCoY - 1)
        case _ => front
      }
      if (newFront == front)
        Rover(position, front)
      else
        Rover(front, newFront)
    }

  }

  final case class Map(xAxisSize: Int, yAxisSize: Int) extends Mars

  final case class CoOrdinate(marsMap: Map, coOrdinate: (Int, Int)) extends Mars {
    def coOrdinate(x: Int, y: Int): CoOrdinate = CoOrdinate(marsMap, (x, y))
  }
}

import Movements._

val newMap = Map(7, 7)



val newRover1 = Rover(CoOrdinate(newMap, (2, 2)), CoOrdinate(newMap, (3, 2)))

newRover1.moveForwards.moveForwards
