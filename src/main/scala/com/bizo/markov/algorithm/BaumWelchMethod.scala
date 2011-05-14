package com.bizo.markov.algorithm

import com.bizo.markov.model._

class BaumWelchMethod(val observations: IndexedSeq[Int]) {

  def apply(model: HiddenMarkovModel): HiddenMarkovModel = {
    import model.{ numberOfStates, numberOfObservations }
    implicit val _ = model

    val step = new BaumWelchStep
    import step._

    new HiddenMarkovModel(numberOfStates, numberOfObservations) {
      val piDenominator = (0 to numberOfStates - 1) map { i =>
        gamma(observations, 1, i)
      } sum

      (0 to numberOfStates - 1) foreach { i =>
        val denominator = (0 to observations.length - 2) map { t =>
          gamma(observations, t, i)
        } sum

        (0 to numberOfStates - 1) foreach { j =>
          val numerator = (0 to observations.length - 2) map { t =>
            xi(observations, t, i, j)
          } sum

          this.A(i, j) = numerator / denominator
        }
      }

      (0 to numberOfStates - 1) foreach { j =>
        val denominator = (0 to observations.length - 2) map { t =>
          gamma(observations, t, j)
        } sum

        (0 to numberOfObservations - 1) foreach { k =>
          val numerator = (0 to numberOfObservations - 2) filter { t =>
            observations(t) == k
          } map { t =>
            gamma(observations, t, j)
          } sum

          this.B(j, k) = numerator / denominator
        }
      }

      (0 to numberOfStates - 1) foreach { i =>
        this.Pi(i) = gamma(observations, 1, i) / piDenominator
      }

    }
  }

}

class BaumWelchStep(implicit val model: HiddenMarkovModel) {
  import model._
  import scala.collection.mutable.{ Map, HashMap }

  private val forward = new ForwardAlgorithm
  private val backward = new BackwardAlgorithm

  import forward.alpha
  import backward.beta

  // xi_t(i,j) - probability of being in state i at time t and state j at time t+1, given all observations through T
  def xi(observations: IndexedSeq[Int], t: Int, i: Int, j: Int): Double = {
    xiNumerator(observations, t, i, j) / xiDenominator(observations, t)
  }

  private val xiNumeratorCache: Map[(IndexedSeq[Int], Int, Int, Int), Double] = new HashMap
  private def xiNumerator(observations: IndexedSeq[Int], t: Int, i: Int, j: Int): Double = {
    val key = (observations, t, i, j)
    if (xiNumeratorCache contains key) {
      xiNumeratorCache(key)
    } else {
      val result = alpha(observations take t, i) * A(i, j) * B(j, observations(t + 1)) * beta(observations drop (t + 1), j)
      xiNumeratorCache(key) = result
      result
    }
  }
  private def xiDenominator(observations: IndexedSeq[Int], t: Int): Double = {
    (0 to numberOfStates - 1) flatMap { i =>
      (0 to numberOfStates - 1) map { j =>
        xiNumerator(observations, t, i, j)
      }
    } sum
  }

  // \gamma_t(i) - probability of being in state i at time t, given observations through t+1
  def gamma(observations: IndexedSeq[Int], t: Int, i: Int): Double = {
    (0 to numberOfStates - 1) map { j =>
      xi(observations, t, i, j)
    } sum
  }

}

object BaumWelchTestMain extends Application {
  val iterations = 2
  val startingModel = new HiddenMarkovModel(2, 2) {
    // init A
    (0 to numberOfStates - 1) foreach { i =>
      (0 to numberOfStates - 1) foreach { j =>
        A(i, j) = 1.0 / numberOfStates
      }
    }

    // init B
    (0 to numberOfObservations - 1) foreach { k =>
      (0 to numberOfStates - 1) foreach { j =>
        B(j, k) = 1.0 / numberOfObservations
      }
    }

    // init Pi
    (0 to numberOfStates - 1) foreach { i =>
      Pi(i) = 1.0 / numberOfStates
    }
  }

  val observations = Array(0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1)

  val method = new BaumWelchMethod(observations)

  var currentModel = startingModel
  Console.out.println("Start")
  currentModel.prettyPrint(Console.out)
  Console.out.println()

  (1 to iterations) foreach { i =>
    Console.out.println("Iteration " + i)
    currentModel = method(currentModel)
    currentModel.prettyPrint(Console.out)
    Console.out.println()
  }
}
