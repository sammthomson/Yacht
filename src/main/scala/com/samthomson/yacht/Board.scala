package com.samthomson.yacht

class UpperBoard() {
  val cells = (1 to Die.NUM_SIDES).map(i => (i, new UpperCell(i))).toMap

  def score = {
    val total = cells.values.map(_.score).flatten.sum
    val bonus = if(total >= UpperBoard.BONUS_THRESHOLD) UpperBoard.BONUS else 0
    total + bonus
  }

  def isComplete = !cells.values.exists(_.score.isEmpty)

  def take(num: Int, roll: Roll) {
    cells(num).take(roll)
  }

  override def toString = "%s\nupper score: %d".format(cells.toList.sortBy(_._1).mkString("\n"), score)
}

object UpperBoard {
  val BONUS = 35
  val BONUS_THRESHOLD = 63
}

class LowerBoard() {
  val cells: Map[String, Cell] = List(
    "ss" -> new Straight(4, 30), // "short straight"
    "ls" -> new Straight(5, 40), // "long straight"
    "fh" -> new FullHouse(), // "full house"
    "3" -> new NofAKind(3), // "three of a kind"
    "4" -> new NofAKind(4), // "four of a kind"
    "c" -> new Chance(), // "chance"
    "y" -> new Yacht() // "yacht"
  ).toMap

  def score = cells.values.map(_.score).flatten.sum

  def isComplete = !cells.values.exists(_.score.isEmpty)

  def take(cellName: String, roll: Roll) {
    cells.get(cellName) match {
      case Some(cell) => cell.take(roll)
      case None => throw new IllegalArgumentException()
    }
  }

  override def toString = "%s\nlower score: %d".format(cells.toList.sortBy(_._1).mkString("\n"), score)
}

class Board {
  val upperBoard = new UpperBoard
  val lowerBoard = new LowerBoard

  def isComplete = upperBoard.isComplete && lowerBoard.isComplete

  def score = upperBoard.score + lowerBoard.score

  def getCell(path: List[String]): Cell = {
    path match {
      case List("u", i) => upperBoard.cells.get(i.toInt).get
      case List("l", name) => lowerBoard.cells.get(name).get
      case _ => throw new IllegalArgumentException
    }
  }

  override def toString = {
    "Upper Board:\n%s\n\nLower Board:\n%s\n\ntotal score: %s".format(upperBoard, lowerBoard, score)
  }
}
