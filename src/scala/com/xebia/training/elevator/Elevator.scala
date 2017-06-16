package com.xebia.training.elevator

case class Elevator(val currentFloor: Int = 0, val pendingRequests: List[Request] = Nil) {

  def move(): Elevator = Elevator(pendingRequests.head.floor, pendingRequests.tail)

  def state = this.pendingRequests match {
    case Nil => Static
    case p@_ => p.head.direction match {
      case "up" => MovingUp
      case "down" => MovingDown
    }
  }

  def request(request: Request): Elevator = request.direction match {
    case "up" => respondToRequest(request, floor => floor >= currentFloor, MovingUp)
    case _ => respondToRequest(request, floor => floor <= currentFloor, MovingDown)
  }

  private def respondToRequest(request: Request, f: Int => Boolean, elevatorState: ElevatorState) = {
    state match {
      case s if s === elevatorState || s === Static => {
        if(f(request.floor)) Elevator(currentFloor, pendingRequests :+ request sortBy (_.floor)) else
          this
      }
      case _ => this
    }
  }
}

case class Request(direction: String, floor: Int)

sealed abstract class ElevatorState {
  implicit def ===(that: ElevatorState) = this == that
}

object MovingUp extends ElevatorState

object MovingDown extends ElevatorState

object Static extends ElevatorState
