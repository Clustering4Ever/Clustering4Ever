package clustering4ever.math.distances

import scala.collection.{immutable, GenSeq}
import clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 **/
object BinaryDistanceUtil
{
	/**
	 * Buil the contingency matrix where for each bite i, j :
	 *   - a is incremented if i = 1, j = 1
	 *   - b is incremented if i = 1, j = 0
	 *   - c is incremented if i = 0, j = 1
	 *   - d is incremented if i = 0, j = 0
	 **/
	def contingencyTable(vector1: immutable.Vector[Int], vector2: immutable.Vector[Int]) =
	{

		val oneByte = 1
		val zeroByte = 0

		def incrementValues(n: Int, a: Int, b: Int, c: Int, d: Int): (Int, Int, Int, Int) =
		{
			if( vector1(n) == oneByte )
			{
				if( vector2(n) == oneByte ) (a + 1, b, c, d)
				else (a, b + 1, c, d)
			}
			else if( vector2(n) == zeroByte ) (a, b, c, d + 1)
			else (a, b, c + 1, d)
		}

		@annotation.tailrec
		def go(n: Int, a: Int, b: Int, c: Int, d: Int): (Int, Int, Int, Int) =
		{
			val (a2, b2, c2, d2) = incrementValues(n, a, b, c, d)
			if( n < vector1.size - 1 ) go(n + 1, a2, b2, c2, d2)
			else (a2, b2, c2, d2)
		}

		go(0, 0, 0, 0, 0)
	}

	/**
	 * Count number of occurence for each binary features
	 * @return Array[(numberOf0, numberOf1)]
	 **/
	def countOccFeat(data: GenSeq[GenSeq[Int]]): GenSeq[(Int, Int)] =
	{
		val nbTotData = data.size
		val nbOne = data.reduce(SumArrays.sumArraysNumerics[Int](_, _))
		val nbZero = nbOne.map(nbTotData - _)
		nbZero.zip(nbOne)
	}

	def genProb2Feat(nbOccFeatTab: GenSeq[(Int, Int)], nbTotData: Int): GenSeq[(Double, Double)] =
	{
		nbOccFeatTab.map{ case (zero, one) =>
		{
			val totDataMinusOne = nbTotData - 1D
			(
				(zero * (zero - 1D)) / (nbTotData * totDataMinusOne),
				(one * (one - 1D)) / (nbTotData * totDataMinusOne)
			)
		}}
	}

}