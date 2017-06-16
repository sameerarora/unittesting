package com.xebia.training

import scala.util.Random


/**
  * Created by sameer on 8/6/17.
  */
object Retry extends App{

  def retry[A, B](f: A => B, arg: A, failure: Boolean, n: Int = 0, maxRetries: Int = 10): B = {
    f(arg) match {
      case false if n < maxRetries => {
        println(s"Command failed attempting ${n + 1} time")
        Thread.sleep(100)
        retry(f, arg, failure, n + 1)
      }
      case a@_ => a
    }
  }

  def foo(i:Int):Boolean = {
    val r: Int = Random.nextInt(10)
    println(s"Value returned is $r")
    i == r
  }

  retry[Int,Boolean](foo, 5, false)

}
