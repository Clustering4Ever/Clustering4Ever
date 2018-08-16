package clustering4ever.spark.clustering.accumulators

import org.apache.spark.util.AccumulatorV2
import scala.collection.{mutable, immutable}
import clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * Accumulator for the maximum value of every real Custom Data
 **/
case class CentroidsScalarAccumulator(initialValue: mutable.HashMap[Int, Seq[Double]], k: Int, dim: Int) extends AccumulatorV2[mutable.HashMap[Int, Seq[Double]], mutable.HashMap[Int, Seq[Double]]]
{
	type CentroidsScalarAccumulatorType = mutable.HashMap[Int, Seq[Double]]

	private var centroidsMap: CentroidsScalarAccumulatorType = initialValue

	def value = centroidsMap

	def isZero = value.forall{ case (clusterID, centroid) => centroid.forall(_ == 0D) }

	def reset: Unit = centroidsMap = mutable.HashMap((0 until k).map( k => (k, Seq.fill(dim)(0D))).toSeq:_*)
	
	def add(m1: CentroidsScalarAccumulatorType): Unit =
	{
		m1.foreach{ case (clusterID, centroid) => centroidsMap(clusterID) = SumArrays.sumArraysNumerics[Double](centroidsMap(clusterID), centroid) }
	}

	def copy: AccumulatorV2[CentroidsScalarAccumulatorType, CentroidsScalarAccumulatorType] = CentroidsScalarAccumulator(value, k, dim)

	def merge(otherAccum: AccumulatorV2[CentroidsScalarAccumulatorType, CentroidsScalarAccumulatorType]): Unit = add(otherAccum.value)

	def addOne(clusterID: Int, centroid: Seq[Double]) =
	{
		centroidsMap(clusterID) = SumArrays.sumArraysNumerics[Double](centroidsMap(clusterID), centroid)
	}

	def set(newInitialValue: CentroidsScalarAccumulatorType) = centroidsMap = newInitialValue
}