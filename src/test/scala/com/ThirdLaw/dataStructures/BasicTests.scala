package com.ThirdLaw.dataStructures

import org.scalatest._

class BasicTests extends FreeSpec with Matchers {
  "BloomFilter" - {
    "should be strongly typed" in {
      assertDoesNotCompile(
        """|val bf = BloomFilter.create[String](falsePositiveProbability = 0.02F, expectedNumberOfElements = 10)
           |bf.put("abc")
           |bf.put(234)""".stripMargin
      )
    }

    val falsePositiveProbability = 0.01F
    val bf = BloomFilter.create[String](falsePositiveProbability = falsePositiveProbability, expectedNumberOfElements = 5)

    val toBeInsertedLst = Seq("testing123", "whiskeyTangoFoxTrot", "lkjas\tasdf!!@#%lkj", "zztop")
    for (input <- toBeInsertedLst) {
      s"should successfully add and then verify as present: $input" in {
        bf.put(input)
        bf.isPresent(input) shouldBe true
      }
    }

    "should successfully maintain the expected false positive probability" in {
      val numSamples = 10000
      val notPresentWordLst: Seq[String] = List.fill(numSamples)(scala.util.Random.alphanumeric.take(30).mkString)
      val isBogusWordPresentResults: Seq[(String, Boolean)] = notPresentWordLst.map(wrd => wrd -> bf.isPresent(wrd))
      val falsePositives: Seq[String] = isBogusWordPresentResults.collect { case (wrd, result) if result => wrd }

      val expectedMaxFalsePositiveCount = Math.ceil(numSamples * falsePositiveProbability).toInt

      falsePositives.length should be <= expectedMaxFalsePositiveCount
    }

    "when overloaded should degenerate into returning true all the time" in {
      val thousandRandomWords = List.fill(1000)(scala.util.Random.alphanumeric.take(8).mkString)
      thousandRandomWords.foreach(bf.put)

      val notPresentWordLst = List.fill(10)(scala.util.Random.alphanumeric.take(15).mkString)
      val isPresentResults = notPresentWordLst.map(bf.isPresent)
      isPresentResults.toSet shouldNot contain(false)
    }

  }

}
