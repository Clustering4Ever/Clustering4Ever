package clustering4ever.spark.clustering.kmeans

import org.apache.spark.util.AccumulatorV2
import scala.collection.mutable
import _root_.clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * Accumulator for the maximum value of every real Custom Data
 **/
case class CentroidsAccumulator(initialValue: mutable.HashMap[Int, Array[Double]], k: Int, dim: Int) extends AccumulatorV2[mutable.HashMap[Int, Array[Double]], mutable.HashMap[Int, Array[Double]]]
{
	type CentroidsAccumulatorType = mutable.HashMap[Int, Array[Double]]

	private var centroidsMap: CentroidsAccumulatorType = initialValue

	def value = centroidsMap

	def isZero = value.forall{ case (clusterID, centroid) => centroid.forall(_ == 0D) }

	def reset: Unit = centroidsMap = mutable.HashMap((0 until k).map( k => (k, Array.fill(dim)(0D))).toSeq:_*)
	
	def add(m1: CentroidsAccumulatorType): Unit =
	{
		m1.foreach{ case (clusterID, centroid) => centroidsMap(clusterID) = SumArrays.sumArraysNumerics(centroidsMap(clusterID), centroid) }
	}

	def copy: AccumulatorV2[CentroidsAccumulatorType, CentroidsAccumulatorType] = CentroidsAccumulator(value, k, dim)

	def merge(otherAccum: AccumulatorV2[CentroidsAccumulatorType, CentroidsAccumulatorType]): Unit = add(otherAccum.value)

	def addOne(clusterID: Int, centroid: Array[Double]) =
	{
		centroidsMap(clusterID) = SumArrays.sumArraysNumerics(centroidsMap(clusterID), centroid)
	}

	def set(newInitialValue: CentroidsAccumulatorType) = centroidsMap = newInitialValue
}