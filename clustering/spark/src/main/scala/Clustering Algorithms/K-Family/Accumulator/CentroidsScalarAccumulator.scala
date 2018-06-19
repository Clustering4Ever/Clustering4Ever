package clustering4ever.spark.clustering.accumulators

import org.apache.spark.util.AccumulatorV2
import scala.collection.{mutable, immutable}
import _root_.clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * Accumulator for the maximum value of every real Custom Data
 **/
case class CentroidsScalarAccumulator(initialValue: mutable.HashMap[Int, immutable.Vector[Double]], k: Int, dim: Int) extends AccumulatorV2[mutable.HashMap[Int, immutable.Vector[Double]], mutable.HashMap[Int, immutable.Vector[Double]]]
{
	type CentroidsScalarAccumulatorType = mutable.HashMap[Int, immutable.Vector[Double]]

	private var centroidsMap: CentroidsScalarAccumulatorType = initialValue

	def value = centroidsMap

	def isZero = value.forall{ case (clusterID, centroid) => centroid.forall(_ == 0D) }

	def reset: Unit = centroidsMap = mutable.HashMap((0 until k).map( k => (k, immutable.Vector.fill(dim)(0D))).toSeq:_*)
	
	def add(m1: CentroidsScalarAccumulatorType): Unit =
	{
		m1.foreach{ case (clusterID, centroid) => centroidsMap(clusterID) = SumArrays.sumArraysNumerics(centroidsMap(clusterID), centroid) }
	}

	def copy: AccumulatorV2[CentroidsScalarAccumulatorType, CentroidsScalarAccumulatorType] = CentroidsScalarAccumulator(value, k, dim)

	def merge(otherAccum: AccumulatorV2[CentroidsScalarAccumulatorType, CentroidsScalarAccumulatorType]): Unit = add(otherAccum.value)

	def addOne(clusterID: Int, centroid: immutable.Vector[Double]) =
	{
		centroidsMap(clusterID) = SumArrays.sumArraysNumerics(centroidsMap(clusterID), centroid)
	}

	def set(newInitialValue: CentroidsScalarAccumulatorType) = centroidsMap = newInitialValue
}