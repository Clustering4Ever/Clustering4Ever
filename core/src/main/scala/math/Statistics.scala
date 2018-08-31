package clustering4ever.stats

import scala.math.{sqrt, pow, min, max}
import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.util.SumArrays
import clustering4ever.scala.kernels.Kernels
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringCommons
import clustering4ever.util.CommonTypes

object Stats extends ClusteringCommons with CommonTypes
{
	type Mean = immutable.Vector[Double]
	type SD = Double

	/**
	 * Compute the standard deviation between vectors and a mean
	 **/
	def sd(vectors: GenSeq[MB[Double]], mean: MB[Double]): SD =
	{
		sqrt({
			var sum = 0D
			vectors.foreach( v => v.indices.foreach( i => sum += pow(v(i) - mean(i), 2) ) ) 
			sum	/ (vectors.size - 1)
		})
	}
	/**
	 * @return min and max for the ith component in reduce style
	 **/
	def obtainIthMinMax(idx: Int, vminMax1: (MB[Double], MB[Double]), vminMax2: (MB[Double], MB[Double])) =
	{
		(
			min(vminMax1._1(idx), vminMax2._1(idx)),
			max(vminMax1._2(idx), vminMax2._2(idx))
		)
	}

	def obtainMinAndMax[S <: GenSeq[Double]](data: GenSeq[S]): (MB[Double], MB[Double]) =
	{
		val dim = data.head.size
		val vectorRange = (0 until dim).toBuffer

		val (minValues, maxValues) = data.map( v => (v.toBuffer, v.toBuffer) ).reduce( (minMaxa, minMaxb) =>
		{
			val minAndMax = vectorRange.map( i => obtainIthMinMax(i, minMaxa, minMaxb) )
			minAndMax.unzip
		})
		(minValues, maxValues)
	}

	def obtainGammaByCluster(v: immutable.Vector[Double], gaussianLawFeaturesSortedByClusterID: immutable.Vector[(ClusterID, (Mean, SD))], πksortedByClusterID: immutable.Vector[Double], metric: ContinuousDistance[GenSeq[Double]] = new Euclidean[GenSeq[Double]](true)) =
	{
		val genProb = gaussianLawFeaturesSortedByClusterID.map{ case (clusterID, (meanC, sdC)) => (clusterID, Kernels.gaussianKernel(v, meanC, 1D / (2 * pow(sdC, 2)), metric)) }

		val averaging = genProb.map{ case (clusterID, prob) => prob * πksortedByClusterID(clusterID) }.sum 

		val gammaByCluster = genProb.map{ case (clusterID, prob) => (clusterID, (πksortedByClusterID(clusterID) * prob) / averaging) }

		gammaByCluster
	}

}