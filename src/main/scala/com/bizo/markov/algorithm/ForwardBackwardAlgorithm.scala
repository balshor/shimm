package com.bizo.markov.algorithm

import com.bizo.markov.model._

class ForwardAlgorithm(implicit val model: HiddenMarkovModel) {
  import model._
  import scala.collection.mutable.{ Map, HashMap }

  private val cache: Map[(IndexedSeq[Int], Int), Double] = new HashMap

  def alpha(observations: IndexedSeq[Int], state: Int): Double = {
    val key = (observations, state)
    if (cache contains key) {
      cache(key)
    } else {
      val result = if (observations.length > 1) {
        ((0 to numberOfStates - 1) map { i =>
          alpha(observations dropRight 1, i) * A(i, state)
        } sum) * B(state, observations.last)
      } else {
        Pi(state) * B(state, 1)
      }
      cache(key) = result
      result
    }
  }

  /** Alias for alpha. */
  def apply(observations: IndexedSeq[Int], state: Int) = alpha(observations, state)

}

class BackwardAlgorithm(implicit val model: HiddenMarkovModel) {
  import model._
  import scala.collection.mutable.{ Map, HashMap }

  private val cache: Map[(IndexedSeq[Int], Int), Double] = new HashMap

  def beta(observations: IndexedSeq[Int], state: Int): Double = {
    val key = (observations, state)
    if (cache contains key) {
      cache(key)
    } else {
      val result = if (observations.length > 0) {
        (0 to numberOfStates - 1) map { j =>
          A(state, j) * B(j, observations.head) * this(observations tail, j)
        } sum
      } else {
        1.0
      }
      cache(key) = result
      result
    }
  }

  /** Alias for beta. */
  def apply(observations: IndexedSeq[Int], state: Int): Double = beta(observations, state)
}

object ForwardBackwardTestMain extends Application {

  implicit val _ = new HiddenMarkovModel(2, 2) {
    // init A
    (0 to numberOfStates - 1) foreach { i =>
      (0 to numberOfStates - 1) foreach { j =>
        A(i, j) = 1.0 / numberOfStates
      }
    }

    // init B
    (0 to numberOfObservations - 1) foreach { k =>
      (0 to numberOfStates - 1) foreach { j =>
        B(j, k) = if (j == k) .75 else .25
      }
    }

    // init Pi
    (0 to numberOfStates - 1) foreach { i =>
      Pi(i) = 1.0 / numberOfStates
    }
  }

  val observations = Array(1)

  Console.println(new ForwardAlgorithm().alpha(observations, 1))
  Console.println(new BackwardAlgorithm().beta(observations, 1))

}
