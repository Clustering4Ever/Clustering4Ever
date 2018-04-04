package clustering4ever.scala.indexes

import _root_.scala.math.{max, log, sqrt}
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.scala.collection.immutable.{HashMap, Map}
import _root_.clustering4ever.scala.indexes.NmiNormalizationNature._

/**
 * @author Beck Gaël
 *
 */
class ExternalIndexes
{
	private def mutualInformationInternal(x: Array[Int], y:Array[Int]) =
	{
		require( x.size == y.size )
		val n = x.size
		val maxX = x.max
		val maxY = y.max

		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

		val count = Array.fill(maxX + 1)(Array.fill(maxY +1)(0D))
		for( i <- x.indices ) count(x(i))(y(i)) += 1D

		val ai = new Array[Double](maxX + 1)
		val bj = new Array[Double](maxY + 1)

		for( m <- maxOneIndices ) for( l <- maxTwoIndices ) ai(m) += count(m)(l)
		for( m <- maxTwoIndices ) for( l <- maxOneIndices ) bj(m) += count(l)(m)


		val nN = ai.reduce(_ + _)
		var hu = 0D
		ai.foreach( v => { val c = v / nN; if( c > 0 ) hu -= c * log(c) } )

		var hv = 0D
		bj.foreach( v => { val c = v / nN; if( c > 0) hv -= c * log(c) } ) 

		var huStrichV = 0D
		for( i <- maxOneIndices ) for( j <- maxTwoIndices ) if( count(i)(j) > 0 ) huStrichV -= count(i)(j) / nN * log( (count(i)(j)) / bj(j) )

		val mi = hu - huStrichV
		(mi, hu, hv)
	}

}

object ExternalIndexes
{
	/**
	 * Compute the mutual information
	 * @return (Mutual Information, entropy x, entropy y)
	 **/
	def mutualInformation(x: Array[Int], y:Array[Int]) =
	{
		(new ExternalIndexes).mutualInformationInternal(x, y)._1
	}

	/**
	 * Compute the normalize mutual entropy
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 **/
	def nmi(x: Array[Int], y:Array[Int], normalization: Normalization = SQRT) =
	{
		val (mi, hu, hv) = (new ExternalIndexes).mutualInformationInternal(x, y)
		val nmi = normalization match
		{
			case SQRT => mi / sqrt(hu * hv)
			case MAX => mi / max(hu, hv)
		}
		nmi
	}

	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 **/
	def prepareList(x: Array[Int]) =
	{
		val indexedValuesMap = x.distinct.zipWithIndex.toMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
}