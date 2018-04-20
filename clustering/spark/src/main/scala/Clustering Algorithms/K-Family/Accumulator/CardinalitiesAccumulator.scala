package clustering4ever.spark.clustering.accumulators

import org.apache.spark.util.AccumulatorV2
import scala.collection.mutable

/**
 * @author Beck Gaël
 **/
case class CardinalitiesAccumulator(initialValue: mutable.HashMap[Int, Long], k: Int) extends AccumulatorV2[mutable.HashMap[Int, Long], mutable.HashMap[Int, Long]]
{
	type CardinalitiesAccumulatorType = mutable.HashMap[Int, Long]

	private var cardinalitiesMap: CardinalitiesAccumulatorType = initialValue

	def value = cardinalitiesMap

	def isZero = value.forall{ case (clusterID, cardinality) => cardinality == 0 }

	def reset: Unit = cardinalitiesMap = mutable.HashMap((0 until k).map( v => (v, 0L)).toSeq:_*)
	
	def add(m1: CardinalitiesAccumulatorType): Unit =
	{
		m1.foreach{ case (clusterID, newCardinality) => cardinalitiesMap(clusterID) = cardinalitiesMap(clusterID) + newCardinality }
	}

	def copy: AccumulatorV2[CardinalitiesAccumulatorType, CardinalitiesAccumulatorType] = CardinalitiesAccumulator(value, k)

	def merge(otherAccum: AccumulatorV2[CardinalitiesAccumulatorType, CardinalitiesAccumulatorType]): Unit = add(otherAccum.value)

	def addOne(clusterID: Int, cardinality: Long) =
	{
		cardinalitiesMap(clusterID) = cardinalitiesMap(clusterID) + cardinality
	}

	def set(newInitialValue: CardinalitiesAccumulatorType) = cardinalitiesMap = newInitialValue
}