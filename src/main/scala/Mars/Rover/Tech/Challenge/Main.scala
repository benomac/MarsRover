package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.Commands.doYouWantToMove
import Mars.Rover.Tech.Challenge.MarsRover.Rover._
import Mars.Rover.Tech.Challenge.MarsRover._


object Main extends App {
  val xAxisLength = scala.io.StdIn.readLine("please enter an x-axis length:- ")
  val yAxisLength = scala.io.StdIn.readLine("please enter an y-axis length:- ")

  val newMap: MapSize = MapSize(xAxisLength.toInt, yAxisLength.toInt)
  val newRover = Rover(newMap)
  println(newRover.currentLocationInfo(newMap))
  doYouWantToMove(newRover, newMap)
}
