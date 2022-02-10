import Mars.Rover.Tech.Challenge.MarsRover.Rover
import Mars.Rover.Tech.Challenge.MarsRover.Rover._
import Mars.Rover.Tech.Challenge._
import org.scalatest.FunSuite

class MarsRoverTechChallengeSpec extends FunSuite {

  val testMap: MapSize = MapSize(10, 10)
  def testRoverCreater(x: Int, y: Int, head: CompassPoint): Rover = {
    Rover(testMap, Coordinate(x, y), head)
  }


  test("Rover movements") {
    val testRover = testRoverCreater(1, 1, East)
    assert(testRover.moveForward.position === Coordinate(2, 1))
    assert(testRover.turnClockWise.heading === South)
    assert(testRover.turnAntiClockWise.heading === North)
  }

  test("map wrap function when moving forward") {
    assert(testRoverCreater(1, 1, North).moveForward.position   === Coordinate(1, 10))
    assert(testRoverCreater(1, 1, West).moveForward.position    === Coordinate(10, 1))
    assert(testRoverCreater(1, 10, South).moveForward.position  === Coordinate(1, 1))
    assert(testRoverCreater(1, 10, West).moveForward.position   === Coordinate(10, 10))
    assert(testRoverCreater(10, 1, North).moveForward.position  === Coordinate(10, 10))
    assert(testRoverCreater(10, 1, East).moveForward.position   === Coordinate(1, 1))
    assert(testRoverCreater(10, 10, South).moveForward.position === Coordinate(10, 1))
    assert(testRoverCreater(10, 10, East).moveForward.position  === Coordinate(1, 10))
  }

  test("turnClockwise") {
    assert(testRoverCreater(1, 1, North).turnClockWise.heading === East)
    assert(testRoverCreater(1, 1, East).turnClockWise.heading === South)
    assert(testRoverCreater(1, 1, South).turnClockWise.heading === West)
    assert(testRoverCreater(1, 1, West).turnClockWise.heading === North)
  }

  test("turnAntiClockwise") {
    assert(testRoverCreater(1, 1, North).turnAntiClockWise.heading === West)
    assert(testRoverCreater(1, 1, East).turnAntiClockWise.heading === North)
    assert(testRoverCreater(1, 1, South).turnAntiClockWise.heading === East)
    assert(testRoverCreater(1, 1, West).turnAntiClockWise.heading === South)
  }

  test("wrapStatus returns Wrapped") {
    val testRover = testRoverCreater(1, 1, North)
    val finishPointCoordinate = Coordinate(10, 1)
    assert(testRover.wrapStatus(testRover.position.x, finishPointCoordinate.x, testRover.theMap.xLength) === Wrapped)
  }

  test("wrapStatus returns NonWrapped") {
    val testRover = testRoverCreater(1, 1, North)
    val finishPointCoordinate = Coordinate(2, 1)
    assert(testRover.wrapStatus(testRover.position.x, finishPointCoordinate.x, testRover.theMap.xLength) === NonWrapped)
  }

}
