package clustering4ever.stats

import scala.math.{sqrt, pow, min, max}
import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.util.SumArrays
import clustering4ever.scala.kernels.Kernels
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringCommons

object Stats extends ClusteringCommons
{
	type Mean = immutable.Vector[Double]
	type SD = Double
	/**
	 * Reduce a matrix into a vector where each component is the sum of its associate column 
	 **/
	def reduceColumns(vectors: immutable.Seq[immutable.Vector[Double]]) =
	{
		vectors.reduce( (a, b) => for( i <- a.indices.toVector ) yield a(i) + b(i) )
	}
	/**
	 * Compute the mean of multiple vectors
	 **/
	def mean(vectors: immutable.Seq[immutable.Vector[Double]]): Mean =
	{
		reduceColumns(vectors).map(_ / vectors.size)
	}
	/**
	 * Compute the standard deviation between vectors and a mean
	 **/
	def sd(vectors: immutable.Seq[immutable.Vector[Double]], mean: immutable.Vector[Double]): SD =
	{
		sqrt(
			vectors.map( v =>
			{
				var sum = 0D
				for( i <- v.indices ) sum += pow(v(i) - mean(i), 2)
				sum
			}).sum / (vectors.size - 1)
		)
	}
	/**
	 * @return min and max for the ith component in reduce style
	 **/
	def obtainIthMinMax(idx: Int, vminMax1: (immutable.Vector[Double], immutable.Vector[Double]), vminMax2: (immutable.Vector[Double], immutable.Vector[Double])) =
	{
		(
			min(vminMax1._1(idx), vminMax2._1(idx)),
			max(vminMax1._2(idx), vminMax2._2(idx))
		)
	}

	def obtainMinAndMax(data: GenSeq[immutable.Seq[Double]]) =
	{
		val dim = data.head.size
		val vectorRange = (0 until dim).toVector

		val (minValues, maxValues) = data.map( v => (v.toVector, v.toVector) ).reduce( (minMaxa, minMaxb) =>
		{
			val minAndMax = for( i <- vectorRange ) yield obtainIthMinMax(i, minMaxa, minMaxb)
			minAndMax.unzip
		})
		(minValues, maxValues)
	}

	def obtainGammaByCluster(v: immutable.Vector[Double], gaussianLawFeaturesSortedByClusterID: immutable.Vector[(ClusterID, (Mean, SD))], πksortedByClusterID: immutable.Vector[Double], metric: ContinuousDistances = new Euclidean(true)) =
	{
		val genProb = gaussianLawFeaturesSortedByClusterID.map{ case (clusterID, (meanC, sdC)) => (clusterID, Kernels.gaussianKernel(v, meanC, 1D / (2 * pow(sdC, 2)), metric)) }

		val averaging = genProb.map{ case (clusterID, prob) => prob * πksortedByClusterID(clusterID) }.sum 

		val gammaByCluster = genProb.map{ case (clusterID, prob) => (clusterID, (πksortedByClusterID(clusterID) * prob) / averaging) }

		gammaByCluster
	}

}