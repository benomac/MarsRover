package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover.Rover._
import Mars.Rover.Tech.Challenge.MarsRover._
import Mars.Rover.Tech.Challenge.Utils.getAllMoves


 object Main extends App {
  val newRover = Rover(MapSize(10, 10), Coordinate(9, 6), East)


  println(getAllMoves(newRover, Coordinate(4, 4)))
  println(newRover.calculateXMoves(newRover, Coordinate(4, 4)))
  println(newRover.calculateYMoves(newRover, Coordinate(4, 4)))


}
