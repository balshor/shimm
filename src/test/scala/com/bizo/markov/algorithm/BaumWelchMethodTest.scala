package com.bizo.markov.algorithm

import org.junit.Assert._
import org.junit._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import com.bizo.markov.model._
import math._

@RunWith(classOf[JUnitRunner])
class BaumWelchMethodTest extends WordSpec with ShouldMatchers {

  "The BaumWelchMethod" should {
    "approximately derive the first iteration model in http://www.indiana.edu/~iulg/moss/hmmcalculations.pdf" in {
      val tolerance = 0.02 // Note: the original paper has only three significant digits of accuracy, may have mathematical errors
      val numberOfStates = 2
      val numberOfObservations = 2

      val startModel = new HiddenMarkovModel(numberOfStates, numberOfObservations) {
        A(0, 0) = 0.3
        A(0, 1) = 0.7

        A(1, 0) = 0.1
        A(1, 1) = 0.9

        B(0, 0) = 0.4
        B(0, 1) = 0.6

        B(1, 0) = 0.5
        B(1, 1) = 0.5

        Pi(0) = 0.85
        Pi(1) = 0.15
      }

      val observations = Map(IndexedSeq(0, 1, 1, 0) -> 10, IndexedSeq(1, 0, 1) -> 20)

      val expectedModel = new HiddenMarkovModel(numberOfStates, numberOfObservations) {
        A(0, 0) = 0.298
        A(0, 1) = 0.702

        A(1, 0) = 0.106
        A(1, 1) = 0.894

        B(0, 0) = 0.357
        B(0, 1) = 0.643

        B(1, 0) = 0.4292
        B(1, 1) = 0.5708

        Pi(0) = 0.846
        Pi(1) = 0.154
      }

      val method = new BaumWelchMethod(observations)
      val computedModel = method.apply(startModel)

      (computedModel.A.data zip expectedModel.A.data) foreach {
        case (row, refRow) =>
          (row zip refRow) map {
            case (entry, refEntry) =>
              assertEquals(entry, refEntry, tolerance)
          }
      }

      (computedModel.B.data zip expectedModel.B.data) foreach {
        case (row, refRow) =>
          (row zip refRow) map {
            case (entry, refEntry) =>
              assertEquals(entry, refEntry, tolerance)
          }
      }

      (computedModel.Pi.weights zip expectedModel.Pi.weights) map {
        case (entry, refEntry) =>
          assertEquals(entry, refEntry, tolerance)
      }
    }

    "derive the same answer as a 10 iteration example using http://www.oga-lab.net/RGM2/func.php?rd_id=HMM:baumWelch" in {
      val tolerance = 0.0001
      val numberOfStates = 2
      val numberOfObservations = 2

      val startModel = new HiddenMarkovModel(numberOfStates, numberOfObservations) {
        A(0, 0) = 0.9
        A(0, 1) = 0.1

        A(1, 0) = 0.1
        A(1, 1) = 0.9

        B(0, 0) = 0.5
        B(0, 1) = 0.5

        B(1, 0) = 0.51
        B(1, 1) = 0.49

        Pi(0) = 0.5
        Pi(1) = 0.5
      }

      val observation = Map(IndexedSeq(1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,0,0,0,1,0,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,0,0,1,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,1,0,1,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,1,1,0,1,1,0,0,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,0,1,0,1,1,0,1,1,0,1,1,1,1,1,0,0,1,0,1,1,1,1,0,1,0,1,1,1,1,1,0,1,1,0,1,1,1,1,0,1,1,1,0,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1,0,0,1,1,0,0,1,1,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,0,0,1,0,1,0,1,1,1,1,1,0,1,1,1,1,0,0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,0,0,1,1,1,1,1,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,0,1,1,0,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,0,1,1,1,1,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,1,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,0,1,1,0,0,1,0,1,0,1,0,0,0,1,1,0,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0) -> 1)

      val expectedModel = new HiddenMarkovModel(numberOfStates, numberOfObservations) {
        A(0, 0) = 9.975280E-01
        A(0, 1) = 0.002472036

        A(1, 0) = 1.842328E-07 
        A(1, 1) = 0.999999816

        B(0, 0) = 0.2515360
        B(0, 1) = 0.7484640

        B(1, 0) = 0.7541825
        B(1, 1) = 0.2458175

        Pi(0) = 0.5
        Pi(1) = 0.5
      }

      val method = new BaumWelchMethod(observation)
      var computedModel = startModel
      (1 to 10) foreach { _ =>
        computedModel = method(computedModel)
      }

      (computedModel.A.data zip expectedModel.A.data) foreach {
        case (row, refRow) =>
          (row zip refRow) map {
            case (entry, refEntry) =>
              assertEquals(entry, refEntry, tolerance)
          }
      }

      (computedModel.B.data zip expectedModel.B.data) foreach {
        case (row, refRow) =>
          (row zip refRow) map {
            case (entry, refEntry) =>
              assertEquals(entry, refEntry, tolerance)
          }
      }

      // Note: the R package used for comparison does not estimate the initial state distribution
    }
  }

}