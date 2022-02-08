package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover.Rover._
import Mars.Rover.Tech.Challenge.MarsRover._


 object Main extends App {
  val newRover = Rover(MapSize(10, 10), Coordinate(1, 1), West)

//
//  println(getAllMoves(newRover, Coordinate(4, 4)))
  println(newRover.calculateXMoveHeadingAfterMoves(Coordinate(4, 4)))





}
