package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover.Rover

object Commands {

  def turnLeftOrRight = ???

  def manualOrAuto = ???

  def moveForwardOrTurn(newRover: Rover, newMap: MapSize): Unit = {
    val forwardOrTurn = scala.io.StdIn.readLine("Do you want to move forwards or turn, F/T? ")

    if (forwardOrTurn.toLowerCase() == "f") {
      val movedOne = newRover.moveForward
      println(movedOne.currentLocationInfo(newMap))
      doYouWantToMove(movedOne, newMap)
    }
    else
      if(forwardOrTurn.toLowerCase() == "t") {
        val leftOrRight = scala.io.StdIn.readLine("Do you want to turn left or right, L/R? ")
        turnLeftOrRight
      }
  }

  def doYouWantToMove(newRover: Rover, newMap: MapSize): Unit = {
    val yesOrNo = scala.io.StdIn.readLine("Do you want to move, Y/N? ")
    if (yesOrNo.toLowerCase() == "y" || yesOrNo.toLowerCase() == "yes")
      moveForwardOrTurn(newRover, newMap)
    else
      if(yesOrNo.toLowerCase() == "n" || yesOrNo.toLowerCase() == "no") {
        println(newRover.currentLocationInfo(newMap))
        doYouWantToMove(newRover, newMap)
      }
      else
        doYouWantToMove(newRover, newMap)
  }
}
