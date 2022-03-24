package Mars.Rover.Tech.Challenge

import Mars.Rover.Tech.Challenge.MarsRover._

import scala.annotation.tailrec


 object Main extends App {

  val newRover = Rover(TheMapSize(3, 3), Coordinate(2, 2), North)


  val moves = Map(Coordinate -> List[Coordinate])

  val finish = Coordinate(1, 1)


   def findRoute(rover: Rover) = {
     @tailrec
     def getAvailableSpaces(rover: Rover, acc: List[Coordinate] = Nil): List[Coordinate] = {
       if (acc.contains(rover.moveForward.position)) acc
       else getAvailableSpaces(rover.turnClockWise, acc :+ rover.moveForward.position)
     }

     def removeCoordinatesIfTheyAreInMapToBeChecked(listOfSpacesAvailable: List[Coordinate], mapToBeChecked: Map[Coordinate, List[Coordinate]]) = {
       listOfSpacesAvailable.filterNot(x => mapToBeChecked.contains(x))
     }

     def addFilteredSpaces(listOfSpacesToAdd: List[Coordinate], moves: Map[Coordinate, List[Coordinate]], ListOfPreviousCo: List[Coordinate]) = {
       moves ++ listOfSpacesToAdd.map(space => (space, ListOfPreviousCo)).toMap
     } //add current Coordinate

   }

 }
