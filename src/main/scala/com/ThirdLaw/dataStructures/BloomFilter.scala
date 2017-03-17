package com.ThirdLaw.dataStructures

import com.ThirdLaw.dataStructures.implementations.BloomFilterImpl

/**
 * BloomFilter API
 */
trait BloomFilter[T] {
  /**
   * adds the given item to the set
   */
  def put(item: T): Unit

  /**
   * Returns if the item present in the set
   * - remember that false positives are possible with BloomFilters
   */
  def isPresent(item: T): Boolean
}

/**
 * API for a hashing API which can generate multiple hashes per input
 */
trait MultiHasher {
  def hashMultiple(input: Any, numHashes: Int): Seq[Long]
}


object BloomFilter {
  /**
   * Creates a BloomFilter for you given an estimated number of elements & desired false positive rate.
   * Exceeding expectedNumberOfElements means that your false positive rate will likely increase
   *
   * @param falsePositiveProbability desired false positive rate (float between 0 and 1)
   * @param expectedNumberOfElements estimated number of elements that will be inserted
   */
  def create[A](falsePositiveProbability: Float, expectedNumberOfElements: Int): BloomFilter[A] =
    new BloomFilterImpl[A](
      falsePositiveProbability = falsePositiveProbability,
      expectedNumberOfElements = expectedNumberOfElements
    )
}

/**
 * Tiny Demo program w/ sample usage
 */
object Testing extends App {

  val bf = BloomFilter.create[String](falsePositiveProbability = 0.02F, expectedNumberOfElements = 10)
  val testStr = "2134"
  bf.put(testStr)
  assert(bf.isPresent(testStr), "isThere")
  assert(!bf.isPresent("XXX"), "XXX isNotThere")
  assert(!bf.isPresent("otherString"), "otherString isNotThere")
  println("All done!")
}
