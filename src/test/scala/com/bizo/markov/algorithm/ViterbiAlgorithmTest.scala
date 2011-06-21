package com.bizo.markov.algorithm

import org.junit.Assert._
import org.junit._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import com.bizo.markov.model._

@RunWith(classOf[JUnitRunner])
class ViterbiAlgorithmTest extends WordSpec with ShouldMatchers {

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
  
  "The Viterbi Algorithm" when {
    "computing delta" should {
      "approximately match random runs of the model" in {
        val observations = Seq(0, 1, 0, 1)
        val endState = 1
        val viterbi = new ViterbiAlgorithm(observations)
        
        val matches = new scala.collection.mutable.HashMap[Seq[Int],Int] 
        (1 to attempts) foreach { _ =>
          val result = model(observations.size)
          if (result._1 == observations && result._2.last == endState) {
            matches(result._2) = matches.getOrElse(result._2,0) + 1
          }
        }
        
        val expectedProbability: Double = matches.values.max.toDouble / attempts
        
        val delta: Double = viterbi(observations.size, endState)._1
        assertEquals(expectedProbability, delta, tolerance)
      }
    }
  }
  
}
