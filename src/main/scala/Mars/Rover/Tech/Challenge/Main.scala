package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover.Rover._
import Mars.Rover.Tech.Challenge.MarsRover._


 object Main extends App {
  val newRover = Rover(MapSize(10, 10), Coordinate(4, 4), South)

//
//  println(getAllMoves(newRover, Coordinate(4, 4)))
//  println(newRover.calculateHeadingAfterXmoves(Coordinate(1, 1)))
  println(newRover.calculateAllMoves(Coordinate(1, 1)))
  val newmap = MapSize(10, 10)
  println(newmap.createListOfCoordinates)





}
