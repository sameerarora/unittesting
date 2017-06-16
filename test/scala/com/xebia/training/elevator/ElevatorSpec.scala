package com.xebia.training.elevator

import org.scalatest.{FlatSpec, Matchers}

class ElevatorSpec extends FlatSpec with Matchers {

  "An Elevator" should "return its current floor number" in {
    val e = Elevator(5)
    e.currentFloor shouldEqual 5
  }

  "An Elevator" should "be able to tell if its static" in {
    val e = Elevator(0)
    e.state shouldEqual Static
  }

  "An Elevator" should "be able to accept a request with direction and floor number" in {
    val e = Elevator(2)
    e.request(Request("up", 5)) shouldEqual Elevator(2, List(Request("up", 5)))
  }

  "An Elevator" should "decline an UP request if coming from a lower floor then current floor" in {
    val e = Elevator(5)
    e.request(Request("up", 2)) shouldEqual e
  }

  "An Elevator" should "accept a DOWN request if coming from a lower floor then current floor" in {
    val e = Elevator(2)
    e.request(Request("down", 5)) shouldEqual e
  }

  "An Elevator" should "accept an up request if coming from a lower floor then current floor if elevator is moving up" in {
    val e = Elevator(2)
    e.request(Request("up", 5)) shouldEqual Elevator(2, List(Request("up", 5)))
  }

  "An Elevator" should "decline an up request if elevator is moving down" in {
    val e = Elevator(2)
    val e1 = e.request(Request("down", 1))
    val e2 = e1.request(Request("down", -1))
    val e3 = e2.move()
    e3.request(Request("up", 5)) shouldEqual e3
  }

  "An Elevator" should "decline a down request if elevator is moving up" in {
    val e = Elevator(5)
    val e1 = e.request(Request("up", 6))
    val e2 = e1.request(Request("up", 7))
    val e3 = e2.move()
    e3.request(Request("down", 2)) shouldEqual e3
  }

  "An Elevator" should "accept a down request if coming from a lower floor and elevator is moving down " in {
    val e = Elevator(5)
    e.request(Request("down", 2)) shouldEqual Elevator(5, List(Request("down", 2)))
  }

  "An Elevator" should "add all accepted requests to pending" in {
    val e = Elevator(5)
    val e1=e.request(Request("down", 2)).request(Request("down", 6))

    e1.pendingRequests.size shouldEqual 1
  }

  "An Elevator" should "move from current floor to nearest accepted request" in {
    val e = Elevator(0)
    val e1=e.request(Request("up", 2)).request(Request("up", 6)).request(Request("up", 7))
    e1.pendingRequests.size shouldEqual 3
    val e2 = e1.move()
    e2.pendingRequests.size shouldEqual 2
    e2.currentFloor shouldEqual 2
    e2.pendingRequests.map(_.floor).contains(6) shouldBe true
    e2.pendingRequests.map(_.floor).contains(7) shouldBe true
  }

  "An Elevator" should "update its state to Moving up if first pending request is to go up" in {
    val e1=Elevator(0).request(Request("up", 2)).request(Request("up", 6)).move()
    e1.state shouldEqual MovingUp
  }

  "An Elevator" should "update its state to Static if there are no pending requests" in {
    val e3 = Elevator(0).request(Request("up", 2)).request(Request("up", 6)).move()
    e3.state shouldEqual MovingUp
    val e4 = e3.move()
    e4.state shouldEqual Static
  }

  "An Elevator" should "Sort requests in order of direction" in {
    val e2 = Elevator(0).request(Request("up", 6)).request(Request("up", 2))
    e2.pendingRequests.head.floor shouldEqual 2
    val e3 = e2.move()
    e3.state shouldEqual MovingUp
  }

}
