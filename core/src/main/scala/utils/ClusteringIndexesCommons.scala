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
            
      RecursivFunctions.goOverMatrix[Double](moi.size - 1, mti.size - 1, 0D, mti.size, computeVal)
    }

    def nmiObtainAi(emptyArr: Array[Double], arr1: Array[Int], arr2: Array[Int], count: Array[Array[Double]]): Array[Double] =
    {
      def computeVal(i: Int, j: Int, arr: Array[Double]): Array[Double] =
      {
        arr(i) += count(i)(j)
        arr
      }

      RecursivFunctions.goOverMatrix[Array[Double]](arr1.size - 1, arr2.size - 1, emptyArr, arr2.size, computeVal)
    }

  	def nmiObtainBj(emptyArr: Array[Double], arr1: Array[Int], arr2: Array[Int], count: Array[Array[Double]]): Array[Double] =
    {
      def computeVal(i: Int, j: Int, arr: Array[Double]): Array[Double] =
      {
        arr(i) += count(j)(i)
        arr
      }

      RecursivFunctions.goOverMatrix[Array[Double]](arr1.size - 1, arr2.size - 1, emptyArr, arr2.size, computeVal)
    }
}