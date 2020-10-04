package reverse.models

import reverse.Rules.evolveState


case class Location(x: Int, y: Int)
object Location {
  def fromInt(position: Int): Location =
    Location(position % 25, Math.floorDiv(position, 25))
}

sealed trait Status
object Status {
  def fromInput(status: String): Status = status match {
    case "0" => Dead
    case "1" => Alive
    case _ => throw new IllegalArgumentException("Oh no!")
  }
}

case object Dead extends Status
case object Alive extends Status

case class Cell(location:Location, status:Status)

case class Board(boardState:Map[Location, Status]){
  val neighbourCells = List((-1,-1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  def lookup(location: Location):Cell = {
    //If we can't find the cell default it to dead
    val status = boardState.getOrElse(location, Dead)

    Cell(location, status)
  }

  def neighbourhood(location: Location): List[Cell] = {

    neighbourCells.map(neighbourhoodVector => lookup(Location(neighbourhoodVector._1 + location.x, neighbourhoodVector._2 + location.y)))

  }

  def evolveBoard(): Board = {
    //TODO: Need to create a thin layer of dead cells around all the alive ones before we evolve state
    val whitespaceToAdd:List[Cell] = boardState.flatMap(x => neighbourhood(x._1).filter(n => !boardState.contains(n.location))).toList
    val boardWithWhiteSpace = whitespaceToAdd.foldRight(boardState)((cell, map) => map + ((cell.location, Dead)))

    //TODO: Only store alive cells in the map
    val newBoard = boardWithWhiteSpace.foldRight(Map[Location, Status]())((x: (Location, Status), y:Map[Location, Status]) => {
      val cell = Cell(x._1, x._2)
      evolveState(cell, neighbourhood = neighbourhood(cell.location)).status match {
        case Alive => y + ((x._1, Alive))
        case Dead => y+ ((x._1, Dead))
      }

    })

    Board(newBoard)
  }
}

object Board {
  def fromInput(input: Map[String, String]): (Board, Board) = {
    val start: Map[Location, Status] = input.collect( a => a match {
      case (key, value) if key.startsWith("start_") => (Location.fromInt(key.replace("start_", "").toInt),
        Status.fromInput(value)
      )
    }
    )

    val stop: Map[Location, Status] = input.collect( a => a match {
      case (key, value) if key.startsWith("stop_") => (Location.fromInt(key.replace("stop_", "").toInt),
        Status.fromInput(value)
      )
    }
    )

    (Board(start), Board(stop))
  }

  def howManyCellsDifferent(boardOne: Board, boardTwo: Board): Int =
    boardOne.boardState.filterNot(l => boardTwo.boardState(l._1) == l._2).toList.length
}
