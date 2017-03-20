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
    if (frame == 10)
      score
    else {
      if (isSpare(rollInFrame)) {
        computeScore(rollInFrame + 2, score + perfect10 + firstBallInTheNextFrame(rollInFrame), frame + 1)
      } else if (isStrike(rollInFrame)) {
        computeScore(rollInFrame + 1, score + perfect10 + nextTwoBallsInTheFrame(rollInFrame), frame + 1)
      } else {
        computeScore(rollInFrame + 2, score + bothBallsInTheFrame(rollInFrame), frame + 1)
      }
    }
  }

  def bothBallsInTheFrame(i: Int) = {
    rolls(i) + rolls(i + 1)
  }

  def nextTwoBallsInTheFrame(i: Int) = {
    rolls(i + 1) + rolls(i + 2)
  }

  def firstBallInTheNextFrame(i: Int): Int = {
    return rolls(i + 2)
  }

  def isStrike(i: Int) = {
    rolls(i) == perfect10
  }

  def isSpare(i: Int) = {
    bothBallsInTheFrame(i) == perfect10
  }
}