package clustering4ever.spark.clustering.accumulators

import org.apache.spark.util.AccumulatorV2
import scala.collection.{mutable, immutable}
import clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * Accumulator for the maximum value of every real Custom Data
 **/
case class CentroidsBinaryAccumulator(initialValue: mutable.HashMap[Int, Seq[Int]], k: Int, dim: Int) extends AccumulatorV2[mutable.HashMap[Int, Seq[Int]], mutable.HashMap[Int, Seq[Int]]]
{
	type CentroidsBinaryAccumulatorType = mutable.HashMap[Int, Seq[Int]]

	private var centroidsMap: CentroidsBinaryAccumulatorType = initialValue

	def value = centroidsMap

	def isZero = value.forall{ case (clusterID, centroid) => centroid.forall(_ == 0) }

	def reset: Unit = centroidsMap = mutable.HashMap((0 until k).map( k => (k, Seq.fill(dim)(0))).toSeq:_*)
	
	def add(m1: CentroidsBinaryAccumulatorType): Unit = m1.foreach{ case (clusterID, centroid) => centroidsMap(clusterID) = SumArrays.sumArraysNumerics[Int](centroidsMap(clusterID), centroid) }

	def copy: AccumulatorV2[CentroidsBinaryAccumulatorType, CentroidsBinaryAccumulatorType] = CentroidsBinaryAccumulator(value, k, dim)

	def merge(otherAccum: AccumulatorV2[CentroidsBinaryAccumulatorType, CentroidsBinaryAccumulatorType]): Unit = add(otherAccum.value)

	def addOne(clusterID: Int, centroid: Seq[Int]) = centroidsMap(clusterID) = SumArrays.sumArraysNumerics[Int](centroidsMap(clusterID), centroid)

	def set(newInitialValue: CentroidsBinaryAccumulatorType) = centroidsMap = newInitialValue
}