package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover.Rover

object Utils {
  def getAllMoves(rover: Rover, moveTo: Coordinate): String = {
    val newCooridiate =
    s"${rover.calculateXMoves(rover, moveTo).mkString} ${rover.calculateYMoves(rover, moveTo).mkString}"
  }
}
