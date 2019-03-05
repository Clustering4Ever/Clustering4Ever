package org.clustering4ever.math
/**
 * @author Beck Gaël
 */
// import scala.collection.GenSeq
/**
 *
 */
object MathC4E extends Serializable {
	/**
	 * The factorial function
	 */
	final def factorial(n: Long): Long = {
	  @annotation.tailrec
	  def go(fact: Long, n: Long): Long = {
	    if(n < 2) fact * n
	    else go(fact * n, n - 1)
	  }
	  if(n == 0) 1 else go(1, n)
	}
	/**
	 * The binomial function
	 */
	final def binom(n: Long, k: Long): Long = {
	  require(0 <= k)
	  @annotation.tailrec 
	  def go(nIter: Long, kIter: Long, ac: Long): Long = {
	    if (kIter > k) ac
	    else go(nIter + 1, kIter + 1, (nIter * ac) / kIter)
	  }
	  if (k == 0 || k == n) 1
	  else if(k > n) 0
	  else go(n - k + 1, 1, 1)
	}
	/**
	 * @return median of an unsorted Seq
	 */
	final def median(seq: Seq[Double]): Double = {
		if(seq.size % 2 == 0) {
			seq.sorted.apply((seq.size + 1) / 2)
		}
		else {
			val ith = ((seq.size + 1) / 2).toInt
			val jth = ith + 1
			val sortedSeq = seq.sorted
			(sortedSeq(ith) + sortedSeq(jth)) / 2
		}
	}

}
