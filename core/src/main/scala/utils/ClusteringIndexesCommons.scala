package clustering4ever.util

import scala.math.log

object ClusteringIndexesCommons
{
	def nmiIn1(arr: Array[Double], s: Double) =
	{
		def computeVal(n: Int, v: Double): Double =
		{
		  val c = arr(n) / s
		  if( c > 0) v - c * log(c) else v
		}

		@annotation.tailrec
		def go(n: Int, v: Double): Double =
		{
		  if( n < arr.size - 1 ) go(n + 1, computeVal(n, v))
		  else computeVal(n, v)
		}
		go(0, 0D)
	}

	def nmiIn2(moi: Array[Int], mti: Array[Int], count: Array[Array[Double]], s: Double, bj: Array[Double]) =
	{
      def computeVal(i: Int, j: Int, v: Double): Double =
      {
        val tmp = count(i)(j)
        if( tmp > 0 ) v - tmp / s * log( tmp / bj(j) ) else v
      }
      
      @annotation.tailrec
      def go(n1: Int, n2: Int, v: Double): Double =
      {
        if( n1 > 0 && n2 > 0 ) go(n1, n2 - 1, computeVal(n1, n2, v))
        else if( n1 > 0 ) go(n1 - 1, mti.size - 1, computeVal(n1, n2, v))
        else if( n1 == 0 && n2 > 0 ) go(n1, n2 - 1, computeVal(n1, n2, v))
        else computeVal(n1, n2, v)
      }
      
      go(moi.size - 1, mti.size - 1, 0D)
  }
}