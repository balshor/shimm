package com.bizo.markov.algorithm

import org.junit.Assert._
import org.junit._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import com.bizo.markov.model._

@RunWith(classOf[JUnitRunner])
class ForwardBackwardAlgorithmTest extends WordSpec with ShouldMatchers {

  implicit val random = new java.util.Random(1234567890000L)
  val tolerance = 0.001
  val attempts = 1000000

  implicit val model = new HiddenMarkovModel(2, 2)
  model.A(0, 0) = 0.33
  model.A(0, 1) = 0.67

  model.A(1, 0) = 0.75
  model.A(1, 1) = 0.25

  model.B(0, 0) = 0.6
  model.B(0, 1) = 0.4

  model.B(1, 0) = 0.45
  model.B(1, 1) = 0.55

  model.Pi(0) = 0.3
  model.Pi(1) = 0.7

  "A forward/backward algorithm" when {
    "computing alpha" should {
      "approximately match random runs of the model" in {
        val observations = IndexedSeq(1, 0)
        val endState = 0
        val algorithm = new ForwardBackwardAlgorithm(observations)

        var numMatches = 0
        (1 to attempts) foreach { _ =>
          val result = model(observations.length)
          if (observations == result._1 && result._2.last == endState) {
            numMatches += 1
          }
        }
        val alphaResult = algorithm.alpha(observations.size, endState)
        val expected = numMatches.toDouble / attempts
        assertEquals(expected, alphaResult, tolerance)
      }
    }

    "computing beta" should {
      "approximately match random runs of the model" in {
        val observations = IndexedSeq(1, 0, 1, 0, 1, 0)
        val startState = 1
        val startTime = 4
        val algorithm = new ForwardBackwardAlgorithm(observations)

        var numMatches = 0
        (1 to attempts) foreach { _ =>
          val result = model(observations.length - startTime + 1, Some(startState))
          if (observations.drop(startTime) == result._1.tail) {
            numMatches += 1
          }
        }
        val betaResult = algorithm.beta(startTime, startState)
        val expected = numMatches.toDouble / attempts
        assertEquals(expected, betaResult, tolerance)
      }
    }

    "computing gamma" should {
      "approximately match random runs of the model" in {
        val observations = IndexedSeq(0, 1, 0, 1)
        val algorithm = new ForwardBackwardAlgorithm(observations)

        val stateCounts = Array.fill(observations.length, model.numberOfStates)(0)
        var matchingObservations = 0
        while (matchingObservations < attempts) {
          val result = model(observations.length)
          if (result._1 == observations) {
            matchingObservations += 1
            result._2.zipWithIndex foreach {
              case (state, time) =>
                stateCounts(time)(state) += 1
            }
          }
        }

        (0 to observations.length - 1) foreach { t =>
          (0 to model.numberOfStates - 1) foreach { i =>
            val expected = stateCounts(t)(i).toDouble / stateCounts(t).sum
            assertEquals("(t=%s, i=%s)".format(t, i), expected, algorithm.gamma(t + 1, i), tolerance)
          }
        }
      }
    }

    "computing xi" should {
      "approximately match random runs of the model" in {
        val observations = IndexedSeq(0, 1, 0)
        val algorithm = new ForwardBackwardAlgorithm(observations)
        val t = 1
        val i = 0
        val j = 1
        
        var matchingTransitions = 0
        var matchingObservations = 0
        while (matchingObservations < attempts) {
          val result = model(observations.length)
          if (result._1 == observations) {
            matchingObservations += 1
            if (result._2(t-1) == i && result._2(t) == j) {
              matchingTransitions += 1
            }
          }
        }
        
        val expected = matchingTransitions.toDouble / matchingObservations
        assertEquals("(t=%s, i=%s, j=%s)".format(t, i, j), expected, algorithm.xi(t, i, j), tolerance)
      }
    }
  }
}