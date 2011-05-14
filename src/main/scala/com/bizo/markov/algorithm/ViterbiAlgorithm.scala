package com.bizo.markov.algorithm

import com.bizo.markov.model._

class ViterbiAlgorithm(implicit val model: HiddenMarkovModel) {
  import model._
  import scala.collection.mutable.{ Map, HashMap }

  private val cache: Map[(IndexedSeq[Int], Int), Double] = new HashMap

  // \delta_t(i)
  def computeProbability(observations: IndexedSeq[Int], state: Int): Double = {
    if (cache contains (observations, state)) {
      cache((observations, state))
    } else {
      val result = if (observations.length == 1) {
        Pi(state) * B(state, 1)
      } else {
        (0 to numberOfStates - 1) map { i =>
          computeProbability(observations dropRight 1, i) * A(i, state) * B(state, observations.last)
        } max
      }
      cache((observations, state)) = result
      result
    }
  }

  // P^*
  def computeProbability(observations: IndexedSeq[Int]): Double = {
    (0 to numberOfStates - 1) map { i =>
      computeProbability(observations, i)
    } max
  }

  // q_T^*
  def apply(observations: IndexedSeq[Int]): IndexedSeq[Int] = {
    if (observations.length == 0) {
      IndexedSeq()
    } else {
      this(observations dropRight 1) :+ ((0 to numberOfStates - 1) map { i =>
        (computeProbability(observations, i), i)
      } max)._2
    }
  }

}

object ViterbiAlgorithmTestMain extends Application {
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
        B(j, k) = if (j != k) 2.0/3 else 1.0/3
      }
    }

    // init Pi
    (0 to numberOfStates - 1) foreach { i =>
      Pi(i) = 1.0 / numberOfStates
    }
  }

  val observations = Array(1, 0, 1, 0)

  println(new ViterbiAlgorithm().apply(observations))
}
