package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover.Rover._
import Mars.Rover.Tech.Challenge.MarsRover._

import scala.annotation.tailrec


 object Main extends App {



  val finish = Coordinate(3, 2)
  val newRover = Rover(MapSize(10, 10), Coordinate(2, 2), North)


  @tailrec
  def findAvailableMoves(rover: Rover,
                         finish: Coordinate,
                         listRoute: List[Int] = List(0),
                         listOfMoves: List[(List[Int], Int, Coordinate)] = Nil,
                         turns: Int = 0): List[Int] = turns match {
   case 4 => findAvailableMoves(Rover(rover.theMap, listOfMoves.head._3, rover.heading), finish, listRoute :+ listOfMoves.head._2, listOfMoves.drop(1))
   case _ =>
    if (rover.position == finish) listRoute
    else
     if (listOfMoves.contains(rover.moveForward.position))
      findAvailableMoves(
       rover.turnClockWise,
       finish,
       listRoute,
       listOfMoves,
       turns + 1)
     else
      findAvailableMoves(
       rover.turnClockWise,
       finish,
       listRoute,
       listOfMoves :+ (listRoute, listOfMoves.length + 1,  rover.moveForward.position),
       turns + 1)
  }


//  def findAvailableMoves(rover: Rover,
//                         finish: Coordinate,
//                         listRoute: List[Int] = List(0),
//                         listOfMoves: List[(List[Int], Int, Coordinate)],
//                         turns: Int): List[Int]= {
//    if (rover.moveForward.position == finish) listRoute
//    else
//      findAvailableMoves(rover.turnClockWise, finish, listRoute)
//  }


   println(findAvailableMoves(newRover, finish, listOfMoves = Nil, turns = 0))
 }
