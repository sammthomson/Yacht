package com.samthomson.yacht

abstract class Cell() {
  var score: Option[Int] = None

  def scoreIfTaken(roll: Roll): Int

  def take(roll: Roll) {
    if (canTake(roll)) {
      score = Some(scoreIfTaken(roll))
    } else throw new IllegalArgumentException
  }

  // works for everything except Yacht
  def canTake(roll: Roll): Boolean = score.isEmpty

  override def toString = score match {
    case Some(s) => s.toString
    case None => "-"
  }
}

class UpperCell(val i: Int) extends Cell {
  override def scoreIfTaken(roll: Roll) = {
    roll.counts.get(i).getOrElse(0) * i
  }
}

class NofAKind(val n: Int) extends Cell {
  override def scoreIfTaken(roll: Roll) = {
    roll.counts.map({case (pip, count) => if (count >= n) count * pip else 0}).sum
  }
}

class FullHouse extends Cell {
  override def scoreIfTaken(roll: Roll) = {
    val countValues = roll.counts.values
    if (countValues.iterator.contains(3) && countValues.iterator.contains(2)) 25 else 0
  }
}

class Chance extends Cell {
  override def scoreIfTaken(roll: Roll) = roll.dice.map(_.pips).sum
}

class Straight(val len: Int, val successScore: Int) extends Cell {
  override def scoreIfTaken(roll: Roll) = {
    if (longestStraightLength(roll) >= len) successScore else 0
  }

  def longestStraightLength(roll: Roll) = {
    val counts = roll.counts
    var currLen = 0
    var maxLen = 0
    (1 to Die.NUM_SIDES).foreach(i => counts.get(i) match {
      case Some(_) => currLen += 1
      case None => {
        maxLen = math.max(maxLen, currLen)
        currLen = 0
      }
    })
    math.max(maxLen, currLen)
  }
}

class Yacht extends Cell {
  override def scoreIfTaken(roll: Roll) = {
    if (isYacht(roll)) score.getOrElse(0) + 50 else 0
  }

  override def canTake(roll: Roll) = score.isEmpty || isYacht(roll)

  def isYacht(roll: Roll) = roll.counts.values.iterator.contains(5)
}
