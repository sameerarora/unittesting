package com.xebia.training.bowling

class BowlingGame {

  val perfect10 = 10

  val rolls = new Array[Int](21)

  var currentIndex = 0

  def roll(pins: Int) {
    rolls(currentIndex) = pins
    currentIndex = currentIndex + 1
  }

  def computeScore(rollInFrame: Int = 0, score: Int = 0, frame: Int = 0): Int = {
    def incrementScore(vals: Int*) = score + vals.toList.sum

    def nextFrame: Int = frame + 1

    frame match {
      case 10 => score
      case _ =>
        if (isSpare(rollInFrame)) {
          computeScore(rollInFrame + 2, incrementScore(perfect10, firstBallInTheNextFrame(rollInFrame)), nextFrame)
        } else if (isStrike(rollInFrame)) {
          computeScore(rollInFrame + 1, incrementScore(perfect10, nextTwoBallsInTheFrame(rollInFrame)), nextFrame)
        } else {
          computeScore(rollInFrame + 2, incrementScore(bothBallsInTheFrame(rollInFrame)), nextFrame)
        }
    }
  }

  def bothBallsInTheFrame(i: Int) = rolls(i) + rolls(i + 1)

  def nextTwoBallsInTheFrame(i: Int) = rolls(i + 1) + rolls(i + 2)

  def firstBallInTheNextFrame(i: Int): Int = rolls(i + 2)

  def isStrike(i: Int) = rolls(i) == perfect10

  def isSpare(i: Int) = bothBallsInTheFrame(i) == perfect10
}