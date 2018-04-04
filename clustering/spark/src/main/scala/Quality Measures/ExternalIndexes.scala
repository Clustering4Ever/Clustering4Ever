package clustering4ever.spark.indexes

import _root_.scala.annotation.meta.param
import _root_.scala.collection.immutable.{HashMap, Map}
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.scala.math.{max, log, sqrt}
import _root_.org.apache.spark.rdd.RDD
import _root_.org.apache.spark.SparkContext
import _root_.clustering4ever.scala.indexes.NmiNormalizationNature._

/**
 * @author Beck Gaël
 *
 */
class ExternalIndexes
{
	private def mutualInformationInternal(@(transient @param) sc: SparkContext, trueAndPredict: RDD[(Int, Int)]) =
	{
		val n = trueAndPredict.count
		val maxX = trueAndPredict.max()(Ordering[Int].on(_._1))._1
		val maxY = trueAndPredict.max()(Ordering[Int].on(_._2))._2

		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

		val accNmi = new NmiAccumulator(Array.fill(maxX + 1)(Array.fill(maxY + 1)(0D)), maxX + 1, maxY + 1)
		sc.register(accNmi, "NmiAccumulator")
		trueAndPredict.foreach{ case (x, y) => accNmi.addOne(x, y) }

		val count = accNmi.value

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
	 * It is advise to cache trueAndPredict before passing it to this method.
	 * @return (Mutual Information, entropy x, entropy y)
	 **/
	def mutualInformation(sc: SparkContext, trueAndPredict: RDD[(Int, Int)]) =
	{
		(new ExternalIndexes).mutualInformationInternal(sc, trueAndPredict)._1
	}

	/**
	 * Compute the normalize mutual entropy
	 * It is advise to cache trueAndPredict before passing it to this method.
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 **/
	def nmi(sc: SparkContext, trueAndPredict: RDD[(Int, Int)], normalization: Normalization = SQRT) =
	{
		val (mi, hu, hv) = (new ExternalIndexes).mutualInformationInternal(sc, trueAndPredict)
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