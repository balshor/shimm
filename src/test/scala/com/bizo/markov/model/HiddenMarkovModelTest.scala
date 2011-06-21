package com.bizo.markov.model

import scala.collection.mutable.{ ArrayBuffer, HashMap }

import org.junit.Assert._
import org.junit._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class HiddenMarkovModelTest extends WordSpec with ShouldMatchers {

  implicit val random = new java.util.Random(1234567890000L)
  val tolerance = 0.001
  val attempts = 1000000

  "An InitialStateDistribution" when {
    "configured and asked to choose initial states" should {
      "produce initial states in proportion to their probability" in {
        val distribution = new InitialStateDistribution(4)
        distribution(0) = 0.5
        distribution(1) = 0.25
        distribution(2) = 0.1875
        distribution(3) = 0.0625

        val counts = Array.fill(4)(0)
        (1 to attempts) foreach { _ =>
          counts(distribution.getInitialState(random.nextDouble)) += 1
        }
        (0 to 3) foreach { index =>
          assertEquals(distribution(index), counts(index).toDouble / attempts, tolerance)
        }
      }
    }

    "normalized" should {
      "keep entries in relative probabilities that sum to 1" in {
        val distribution = new InitialStateDistribution(4)
        distribution(0) = 5000
        distribution(1) = 2500
        distribution(2) = 1875
        distribution(3) = 625

        distribution.normalize()

        assertEquals(0.5, distribution(0), tolerance)
        assertEquals(0.25, distribution(1), tolerance)
        assertEquals(0.1875, distribution(2), tolerance)
        assertEquals(0.0625, distribution(3), tolerance)
      }
    }
  }

  "ObservationProbabilities" when {
    "configured and asked to choose observations" should {
      "produce observations in proportion to their probability" in {
        val observationProbabilities = new ObservationProbabilities(4, 2)

        observationProbabilities(0, 0) = 0.75
        observationProbabilities(0, 1) = 0.25

        observationProbabilities(1, 0) = 0.66
        observationProbabilities(1, 1) = 0.34

        observationProbabilities(2, 0) = 0.875
        observationProbabilities(2, 1) = 0.125

        observationProbabilities(3, 0) = 0.9
        observationProbabilities(3, 1) = 0.1

        (0 to 3) foreach { state =>
          val counts = Array.fill(2)(0)
          (1 to attempts) foreach { _ =>
            counts(observationProbabilities.getObservation(state, random.nextDouble)) += 1
          }
          (0 to 1) foreach { observation =>
            assertEquals(observationProbabilities(state, observation), counts(observation).toDouble / attempts, tolerance)
          }
        }
      }
    }

    "normalized" should {
      "keep relative probabilities that sum to 1" in {
        val observationProbabilities = new ObservationProbabilities(4, 2)
        observationProbabilities(0, 0) = 75
        observationProbabilities(0, 1) = 25

        observationProbabilities(1, 0) = 66
        observationProbabilities(1, 1) = 34

        observationProbabilities(2, 0) = 875
        observationProbabilities(2, 1) = 125

        observationProbabilities(3, 0) = 9
        observationProbabilities(3, 1) = 1
        
        observationProbabilities.normalize()

        assertEquals(0.75, observationProbabilities(0, 0), tolerance)
        assertEquals(0.25, observationProbabilities(0, 1), tolerance)
        assertEquals(0.66, observationProbabilities(1, 0), tolerance)
        assertEquals(0.34, observationProbabilities(1, 1), tolerance)
        assertEquals(0.875, observationProbabilities(2, 0), tolerance)
        assertEquals(0.125, observationProbabilities(2, 1), tolerance)
        assertEquals(0.9, observationProbabilities(3, 0), tolerance)
        assertEquals(0.1, observationProbabilities(3, 1), tolerance)
      }
    }
  }

  "TransitionProbabilities" when {
    "configured and asked to choose transitions" should {
      "produce transitions in proportion to their probability" in {
        val transitionProbabilities = new TransitionProbabilities(2)

        transitionProbabilities(0, 0) = 0.125
        transitionProbabilities(0, 1) = 0.875
        transitionProbabilities(1, 0) = 0.75
        transitionProbabilities(1, 1) = 0.25

        (0 to 1) foreach { state =>
          val counts = Array.fill(2)(0)
          (1 to attempts) foreach { _ =>
            counts(transitionProbabilities.getNextState(state, random.nextDouble)) += 1
          }
          (0 to 1) foreach { nextState =>
            assertEquals(transitionProbabilities(state, nextState), counts(nextState).toDouble / attempts, tolerance)
          }
        }
      }
    }

    "normalized" should {
      "keep relative probabilities that sum to 1" in {
        val transitionProbabilities = new TransitionProbabilities(2)

        transitionProbabilities(0, 0) = 125
        transitionProbabilities(0, 1) = 875
        transitionProbabilities(1, 0) = 750
        transitionProbabilities(1, 1) = 250

        transitionProbabilities.normalize()

        assertEquals(0.125, transitionProbabilities(0, 0), tolerance)
        assertEquals(0.875, transitionProbabilities(0, 1), tolerance)
        assertEquals(0.75, transitionProbabilities(1, 0), tolerance)
        assertEquals(0.25, transitionProbabilities(1, 1), tolerance)
      }
    }
  }

  "HiddenMarkovModels" when {
    "configured and asked to choose a random path" should {
      "produce random paths in proportion to their probabilities" in {
        implicit val model = new HiddenMarkovModel(2, 2)
        model.A(0, 0) = 0.5
        model.A(0, 1) = 0.5

        model.A(1, 0) = 0.5
        model.A(1, 1) = 0.5

        model.B(0, 0) = 0.5
        model.B(0, 1) = 0.5

        model.B(1, 0) = 0.5
        model.B(1, 1) = 0.5

        model.Pi(0) = 0.5
        model.Pi(1) = 0.5

        val numTransitions = 4
        val result = model(numTransitions)
        assertEquals(numTransitions, result._1.size)
        result._2 foreach { n => assertTrue(Seq(0, 1) contains n) }

        val counts = new HashMap[(Seq[Int], Seq[Int]), Int]
        (1 to attempts) foreach { _ =>
          val result = model(numTransitions)
          counts(result) = counts.getOrElse(result, 0) + 1
        }
        val expectedProbability = 1.0 / math.pow(2, 2 * numTransitions)
        counts.values foreach { value =>
          assertEquals(expectedProbability, value.toDouble / attempts, tolerance)
        }
      }
    }
  }

}