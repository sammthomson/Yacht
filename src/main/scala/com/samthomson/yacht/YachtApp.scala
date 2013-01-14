package com.samthomson.yacht

import java.lang.String
import java.io.{InputStreamReader, BufferedReader}

object Util {
  @annotation.tailrec
  def retry[T](n: Int)(fn: => T): T = {
    val r = try { Some(fn) } catch { case e: Exception if n > 1 => None }
    r match {
      case Some(x) => x
      case None => retry(n - 1)(fn)
    }
  }
}

case class Player(id: Int)

class PlayerState(val player: Player, val playerInput: PlayerInput) {
  val board = new Board

  def isComplete = board.isComplete

  def score = board.score

  def takeTurn() {
    println("Player %s's turn".format(player.id))
    println("Board:")
    println(this)
    var roll = Roll.firstRoll
    println(roll)
    var action: PlayerAction = null
    while(!action.isInstanceOf[Take]) {
      action = playerInput.prompt(this, roll)
      action match {
        case Keep(keeping) => {
          roll = roll.keepAndRoll(keeping)
          println(roll)
        }
        case Take(cell) => {
          cell.take(roll)
          println("Score += %d".format(cell.score.get))
        }
      }
    }
  }

  override def toString = board.toString
}

class GameState(val numPlayers: Int, playerInput: PlayerInput) {
  val playerStates: List[PlayerState] = (1 to numPlayers).map(i => new PlayerState(new Player(i), playerInput)).toList

  def isComplete = playerStates.forall(_.board.isComplete)
}

case class Die(pips: Int)
object Die {
  val NUM_SIDES = 6
}

case class Roll(dice: List[Die], rollNum: Int) {
  lazy val counts: Map[Int, Int] = {
    val pips = dice.map(_.pips)
    pips.groupBy(identity).map({case (i, list) => (i -> list.length)})
  }

  def keepAndRoll(keeping: List[Int]): Roll = {
    val newDice = keeping.map(dice(_)) ++ Roll.rollDice(YachtApp.NUM_DICE - keeping.length)
    Roll(newDice, rollNum + 1)
  }

  lazy val isComplete = rollNum == YachtApp.ROLLS_PER_TURN

  override def toString = "Roll number %s:\n%s".format(rollNum, dice.zipWithIndex.mkString("\n"))
}

object Roll {
  def firstRoll = {
    val dice = rollDice(YachtApp.NUM_DICE)
    Roll(dice, 1)
  }

  def rollDice(numDice: Int) = List.fill(numDice)(Die((math.random * Die.NUM_SIDES).toInt + 1))
}

sealed abstract class PlayerAction
case class Keep(keeping: List[Int]) extends PlayerAction
case class Take(cell: Cell) extends PlayerAction

trait PlayerInput {
  def prompt(playerState: PlayerState, roll: Roll): PlayerAction
}
class TextInput extends PlayerInput {
  private val input = new BufferedReader(new InputStreamReader(System.in))

  def prompt(state: PlayerState, roll: Roll): PlayerAction = {
    Util.retry(TextInput.NUM_RETRIES)({ promptOnce(state, roll) })
  }

  def promptOnce(state: PlayerState, roll: Roll): PlayerAction = {
    println(if (roll.isComplete) "Take?" else "Keep or Take?")
    val line = input.readLine()
    val fields = line.split(" ").toList
    fields match {
      case("k" :: rest) => if (!roll.isComplete) Keep(rest.map(_.toInt)) else throw new IllegalArgumentException
      case("t" :: rest) => Take(state.board.getCell(rest))
      case _ => throw new IllegalArgumentException
    }
  }
}
object TextInput {
  val NUM_RETRIES: Int = 3
}

class YachtApp(val playerInput: PlayerInput) {
  def run() {
    val gameState = new GameState(2, playerInput)
    while(!gameState.isComplete) {
      gameState.playerStates.filter(!_.board.isComplete).foreach(_.takeTurn())
    }
    displayResults(gameState)
  }

  private def displayResults(state: GameState) {
    val scores = state.playerStates.map(s => (s.player.id, s.score))
    scores.foreach({case (id, score) => println("Player %d: %d".format(id, score))})
    val winner = scores.maxBy(_._2)
    winner match {case (id, score) => println("Player %d wins!".format(id))}
  }
}

object YachtApp {
  val NUM_DICE = 5
  val ROLLS_PER_TURN = 3

  def main(args: Array[String]) {
    val app = new YachtApp(new TextInput)
    app.run()
  }
}
