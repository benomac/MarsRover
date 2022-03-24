import Mars.Rover.Tech.Challenge.{Coordinate, TheMapSize}

val newMap = TheMapSize(5, 5)

val start = Coordinate(1, 1)

val finish = Coordinate(5, 5)

val coordinateList = newMap.createListOfCoordinates
coordinateList.foreach(println)

