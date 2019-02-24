package org.clustering4ever.math
/**
 * @author Beck Gaël
 */
object MathC4E extends Serializable {
	/**
	 * The factorial function
	 */
	def factorial(n: Int): Int = {
	  @annotation.tailrec
	  def go(fact: Int, n: Int): Int = {
	    if(n < 2) fact * n
	    else go(fact * n, n - 1)
	  }
	  if(n == 0) 1 else go(1, n)
	}
	/**
	 * The binomial function
	 */
	def binom(n: Int, k: Int): Int = {
	  require(0 <= k && k <= n)
	  @annotation.tailrec 
	  def go(nIter: Int, kIter: Int, ac: Int): Int = {
	    if (kIter > k) ac
	    else go(nIter + 1, kIter + 1, (nIter * ac) / kIter)
	  }
	  if (k == 0 || k == n) 1
	  else go(n - k + 1, 1, 1)
	}

}
