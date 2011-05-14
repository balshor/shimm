package com.bizo.markov.model

class HiddenMarkovModel(
  val numberOfStates: Int,
  val numberOfObservations: Int) {

  val A = new StateTransitionMatrix(numberOfStates)
  val B = new ObservationProbabilities(numberOfStates, numberOfObservations)
  val Pi = new InitialStateDistribution(numberOfStates)

  def prettyPrint(w: java.io.PrintStream) {
    import w.{ print, println }
    (0 to numberOfStates - 1) foreach { row =>
      if (row == 0) {
        print("A: [ ")
      } else {
        print("     ")
      }
      print(A.data(row) mkString("[", " ", "]"))
      if (row == numberOfStates - 1) {
        print(" ]")
      }
      println
    }
    (0 to numberOfStates - 1) foreach { row =>
      if (row == 0) {
        print("B: [ ")
      } else {
        print("     ")
      }
      print(B.data(row) mkString("[", " ", "]"))
      if (row == numberOfStates - 1) {
        print(" ]")
      }
      println      
    }
    print("Pi:  ")
    print(Pi.data mkString("[", " ", "]"))
    println
  }
}

/**
 * A = \{ a_{ij} \} State transition probability matrix.
 * a_{ij} = P(q_{t+1} = S_j | q_t = S_i)
 */
class StateTransitionMatrix(val numberOfStates: Int) {
  val data: Array[Array[Double]] = Array.ofDim(numberOfStates, numberOfStates)

  def apply(i: Int, j: Int): Double = data(i)(j)
  def update(i: Int, j: Int, value: Double) = {
    data(i)(j) = value
  }
}

/**
 * B = \{ b_j(k) \} observation probabilities.
 * b_j(k) = P(v_k at t | q_t = S_j)
 */
class ObservationProbabilities(numberOfStates: Int, numberOfObservations: Int) {
  val data: Array[Array[Double]] = Array.ofDim(numberOfStates, numberOfObservations)

  def apply(stateIndex: Int, observationIndex: Int): Double = data(stateIndex)(observationIndex)
  def update(stateIndex: Int, observationIndex: Int, value: Double) = {
    data(stateIndex)(observationIndex) = value
  }

}

/**
 * \pi Initial state probability distribution.
 * \pi_i = P(q_1 = S_i)
 */
class InitialStateDistribution(val numberOfStates: Int) {
  val data: Array[Double] = Array.ofDim(numberOfStates)

  def apply(i: Int) = data(i)
  def update(i: Int, value: Double) = {
    data(i) = value
  }
}
