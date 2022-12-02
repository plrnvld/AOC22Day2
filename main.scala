import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    var allPoints = 0
    val source = Source.fromFile("Input.txt")
    for (line <- source.getLines()) {
        allPoints = allPoints + score(translateUsingOutcome(line))
    }

    def translateUsingOutcome(line: String) : String = {
        line(2) match {
            case 'X' => makeLosing(line(0))
            case 'Y' => makeTied(line(0))
            case 'Z' => makeWinning(line(0))
        }        
    }

    def makeLosing(move: Char): String = {
        move match {
            case 'A' => "A Z"
            case 'B' => "B X"
            case 'C' => "C Y"
        }
    }

    def makeTied(move: Char): String = {
        move match {
            case 'A' => "A X"
            case 'B' => "B Y"
            case 'C' => "C Z"
        }
    }

    def makeWinning(move: Char): String = {
        move match {
            case 'A' => "A Y"
            case 'B' => "B Z"
            case 'C' => "C X"
        }
    }

    def score(line: String): Int = {        
        val winPoints = line match {
            case "A X" => 3
            case "A Y" => 6
            case "A Z" => 0
            case "B X" => 0
            case "B Y" => 3
            case "B Z" => 6
            case "C X" => 6
            case "C Y" => 0
            case "C Z" => 3
        }
        
        val movePoints = line(2) match {
            case 'X' => 1
            case 'Y' => 2
            case 'Z' => 3 
        }

        winPoints + movePoints
    }
  
    source.close()

    println(allPoints)
  }
}
