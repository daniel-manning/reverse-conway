package reverse

import reverse.models.{Alive, Dead, Cell}

object Rules {

  def evolveState(cell:Cell, neighbourhood: List[Cell]):Cell = {
    cell match {
      case Cell(_, Alive) => countAliveInNeighbourhood(neighbourhood) match {
        case n if n < 2 => Cell(location = cell.location, Dead)
        case n if n <= 3 => Cell(location = cell.location, Alive)
        case n if n > 3 => Cell(location = cell.location, Dead)
      }

      case Cell(_, Dead) => countAliveInNeighbourhood(neighbourhood) match {
        case n if n == 3 => Cell(location = cell.location, Alive)
        case _ => Cell(location = cell.location, Dead)
      }
    }
  }

  private def countAliveInNeighbourhood(neighbourhood: List[Cell]): Int = {
    neighbourhood.count(_.status == Alive)
  }


}