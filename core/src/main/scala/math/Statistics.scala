package clustering4ever.stats

import _root_.scala.math.{sqrt, pow, min, max}
import _root_.clustering4ever.util.SumArrays
import _root_.scala.collection.mutable
import _root_.clustering4ever.scala.kernels.Kernels
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean

object Stats
{
	type Mean = Array[Double]
	type SD = Double
	type ClusterID = Int

	/**
	 * Reduce a matrix into a vector where each component is the sum of its associate column 
	 **/
	def reduceColumns(vectors: Array[Array[Double]]) =
	{
		vectors.reduce( (a, b) => for( i <- a.indices.toArray ) yield (a(i) + b(i)) )
	}
	/**
	 * Compute the mean of multiple vectors
	 **/
	def mean(vectors: Array[Array[Double]]): Mean =
	{
		reduceColumns(vectors).map(_ / vectors.size)
	}
	/**
	 * Compute the standard deviation between vectors and a mean
	 **/
	def sd(vectors: Array[Array[Double]], mean: Array[Double]): SD =
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
	def obtainIthMinMax(idx: Int, vminMax1: (Array[Double], Array[Double]), vminMax2: (Array[Double], Array[Double])) =
	{
		(
			min(vminMax1._1(idx), vminMax2._1(idx)),
			max(vminMax1._2(idx), vminMax2._2(idx))
		)
	}

	def obtainMinAndMax(data: Seq[Array[Double]]) =
	{
		val dim = data.head.size
		val vectorRange = (0 until dim).toArray

		val (minValues, maxValues) = data.map( v => (v, v) ).reduce( (minMaxa, minMaxb) =>
		{
			val minAndMax = for( i <- vectorRange ) yield (obtainIthMinMax(i, minMaxa, minMaxb))
			minAndMax.unzip
		})
		(minValues, maxValues)
	}

	def obtainGammaByCluster(v: Array[Double], gaussianLawFeaturesSortedByClusterID: Array[(ClusterID, (Mean, SD))], πksortedByClusterID: Array[Double], metric: ContinuousDistances = new Euclidean(true)) =
	{
		val genProb = gaussianLawFeaturesSortedByClusterID.map{ case (clusterID, (meanC, sdC)) => (clusterID, Kernels.gaussianKernel(v, meanC, 1D / (2 * pow(sdC, 2)), metric)) }

		val averaging = genProb.map{ case (clusterID, prob) => prob * πksortedByClusterID(clusterID) }.sum 

		val gammaByCluster = genProb.map{ case (clusterID, prob) => (clusterID, (πksortedByClusterID(clusterID) * prob) / averaging) }

		gammaByCluster
	}

}