package org.clustering4ever.math.distances
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import org.clustering4ever.util.SumVectors
import org.clustering4ever.vectors.BinaryVector
/**
 *
 */
object BinaryDistanceUtil extends Serializable {
	/**
	 * Buil the contingency matrix where for each bite i, j :
	 *   - a is incremented if i = 1, j = 1
	 *   - b is incremented if i = 1, j = 0
	 *   - c is incremented if i = 0, j = 1
	 *   - d is incremented if i = 0, j = 0
	 */
	def contingencyTable[V <: Seq[Int]](vector1: V, vector2: V): (Int, Int, Int, Int) = {

		val oneBite = 1
		val zeroBite = 0

		def incrementValues(n: Int, a: Int, b: Int, c: Int, d: Int): (Int, Int, Int, Int) = {
			if(vector1(n) == oneBite) {
				if(vector2(n) == oneBite) (a + 1, b, c, d)
				else (a, b + 1, c, d)
			}
			else if(vector2(n) == zeroBite) (a, b, c, d + 1)
			else (a, b, c + 1, d)
		}

		@annotation.tailrec
		def go(n: Int, a: Int, b: Int, c: Int, d: Int): (Int, Int, Int, Int) = {
			val (a2, b2, c2, d2) = incrementValues(n, a, b, c, d)
			if(n < vector1.size - 1) go(n + 1, a2, b2, c2, d2)
			else (a2, b2, c2, d2)
		}

		go(0, 0, 0, 0, 0)
	}
	/**
	 * Buil the contingency matrix where for each bite i, j :
	 *   - a is incremented if i = 1, j = 1
	 *   - b is incremented if i = 1, j = 0
	 *   - c is incremented if i = 0, j = 1
	 *   - d is incremented if i = 0, j = 0
	 */
	def contingencyTable[V <: Seq[Int]](vector1: BinaryVector[V], vector2: BinaryVector[V]): (Int, Int, Int, Int) = contingencyTable(vector1.vector, vector2.vector)

	/**
	 * Count number of occurence for each binary features
	 * @return Array[(numberOf0, numberOf1)]
	 */
	def countOccFeat(data: Seq[Seq[Int]]): Seq[(Int, Int)] = {
		import org.clustering4ever.util.VectorsAddOperationsImplicits._
		val nbTotData = data.size
		val nbOne = data.reduce(SumVectors.sumVectors(_, _))
		val nbZero = nbOne.map(nbTotData - _)
		nbZero.zip(nbOne)
	}

	def genProb2Feat(nbOccFeatTab: Seq[(Int, Int)], nbTotData: Int): Seq[(Double, Double)] = {
		nbOccFeatTab.map{ case (zero, one) =>
			val totDataMinusOne = nbTotData - 1D
			val product = nbTotData * totDataMinusOne
			(
				(zero * (zero - 1D)) / product,
				(one * (one - 1D)) / product
			)
		}
	}

}