package com.ThirdLaw.dataStructures

import scala.util.Try

package object implementations {

  trait MultiHasherImpl extends MultiHasher {

    override def hashMultiple(input: Any, numHashes: Int): Seq[Long] = {
      require(numHashes > 0, s"numHashes argument to hashMultiple must be > 0! Got $numHashes")
      val objectHashCodeAsByteArray = objectToHashedByteArray(input)
      val firstHash = hashTheFirst(objectHashCodeAsByteArray)
      val secondHash = hashTheSecond(objectHashCodeAsByteArray)
      //technique described here: http://spyced.blogspot.com/2009/01/all-you-ever-wanted-to-know-about.html
      (1 until numHashes).foldLeft(Seq(firstHash.toLong)) { case (lst, numHash) =>
        lst :+ (firstHash.toLong + (numHash.toLong * secondHash.toLong))
      }
    } ensuring(lst => lst.length == numHashes, "Internal Error! Incorrect number of hashes returned from hashMultiple!")

    private def objectToHashedByteArray(obj: Any): Array[Byte] = {
      val intHashCode = obj.hashCode()
      intToByteArray(intHashCode)
    }

    private def intToByteArray(i: Int): Array[Byte] = {
      import scala.math.BigInt
      BigInt(i).toByteArray
    }

    private def hashTheFirst: Array[Byte] => Int = hashWithSeed(_, seed = scala.util.hashing.MurmurHash3.arraySeed)

    private def hashTheSecond: Array[Byte] => Int = hashWithSeed(_, seed = scala.util.hashing.MurmurHash3.productSeed)

    private def hashWithSeed(byteAry: Array[Byte], seed: Int): Int =
      scala.util.hashing.MurmurHash3.arrayHash(byteAry, seed)

  }


  class BloomFilterImpl[A](falsePositiveProbability: Float, expectedNumberOfElements: Int)
    extends BloomFilter[A] with MultiHasherImpl {
    require(falsePositiveProbability > 0F && falsePositiveProbability <= 1F, s"Invalid falsePosProbability of " +
      s"$falsePositiveProbability. It must be a proper probability between 0 & 1")
    require(expectedNumberOfElements >= 0, s"Invalid expectedNumberOfElems $expectedNumberOfElements. Must be >=0")

    private val NUM_HASHES: Int = calculateNumberOfHashesFromFalseProbability(falsePositiveProbability)
    private val BITS_PER_ELEMENT: Int = calculateBitsPerElement(falsePositiveProbability, expectedNumberOfElements)
    private val LENGTH: Int = BITS_PER_ELEMENT * expectedNumberOfElements
    private val bitSet = new scala.collection.mutable.BitSet(initSize = LENGTH)

    private def calculateNumberOfHashesFromFalseProbability(falsePosProbability: Float): Int = {
      //see math here: https://en.wikipedia.org/wiki/Bloom_filter#Optimal_number_of_hash_functions
      val k = -(Math.log(falsePosProbability) / Math.log(2))
      Math.ceil(k).toInt //Math.ceil to ensure we end up with k >=1
    }

    private def calculateBitsPerElement(falsePosProbability: Float, expectedNumberOfElems: Int): Int = {
      //see math here: https://en.wikipedia.org/wiki/Bloom_filter#Optimal_number_of_hash_functions
      val m = (-expectedNumberOfElems * Math.log(falsePosProbability)) / math.pow(Math.log(2), 2)
      Math.ceil(m).toInt
    }

    override def put(input: A): Unit = {
      val hashes = hashMultiple(input, NUM_HASHES)
      val indices = hashes.map(moduloToIndex)
      indices.foreach(index => bitSet += index)
    }

    private def moduloToIndex(index: Long): Int = Try {
      val positiveIndex = Math.abs(index)
      val moduloIndex = positiveIndex % LENGTH
      val indexAsInt = moduloIndex.toInt
      indexAsInt
    }.getOrElse(throw new RuntimeException(s"Invalid index $index - probably great than an Int?"))

    override def isPresent(item: A): Boolean = {
      hashMultiple(item, NUM_HASHES).map(moduloToIndex).forall(index => bitSet.contains(index))
    }

  }


}
