package reverse

import java.io.File

import com.github.tototoshi.csv._
import reverse.Config.trainingFileLocation
import reverse.models.Board

object Loader extends App {

  val reader = CSVReader.open(new File(trainingFileLocation))


  val oneMove = reader.toStreamWithHeaders.filter(map => map("delta") equals "1").toList
  val topline = oneMove.head
  println(s"topline: ${topline.toList.sorted}")

  println(s"There are ${oneMove.length} examples of distance one moves")
  println(s"There are 50000 examples in total")

  val (start, stop) = Board.fromInput(topline)

  val stop_evaluated = start.evolveBoard()

  val difference = Board.howManyCellsDifferent(stop, stop_evaluated)
  println(s"After transformation there are $difference cells different")


}
