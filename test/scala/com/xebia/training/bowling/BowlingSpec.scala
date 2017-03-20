package com.xebia.training.bowling

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}


class BowlingSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  var bowlingGame: BowlingGame = _

  override protected def beforeEach(): Unit = {
    bowlingGame = new BowlingGame
  }

  "A Bowling Game " should "score 0 in a gutter game" in {
    rollMany(20, 0)
    bowlingGame.computeScore() should equal(0)
  }

  "A Bowling Game " should "score 20 in a complex game" in {
    rollMany(18, 0)
    bowlingGame.roll(5)
    bowlingGame.roll(5)
    bowlingGame.roll(10)
    bowlingGame.computeScore() should equal(20)
  }

  "A Bowling Game " should "handle spare " in {
    bowlingGame.roll(10)
    bowlingGame.roll(4)
    bowlingGame.roll(3)
    rollMany(16, 0)
    bowlingGame.computeScore() should equal(24)
  }

  "A Bowling Game " should "score 300 in a perfect game" in {
    rollMany(12, 10);
    bowlingGame.computeScore() should equal(300)
  }

  def rollMany(n: Int, pins: Int) {
    (1 to n).foreach(i => bowlingGame.roll(pins))
  }
}
