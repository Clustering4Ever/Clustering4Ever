package clustering4ever.stats

import scala.math.{sqrt, pow, min, max, Pi}
import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.util.SumVectors
import clustering4ever.scala.kernels.Kernels
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringCommons
import clustering4ever.util.CommonTypes

object Stats extends ClusteringCommons with CommonTypes
{
	type Mean = Seq[Double]
	type SD = Seq[Double]

	/**
	 * @return the standard deviation between vectors and a mean
	 **/
	def sd(vectors: Seq[Seq[Double]], mean: Seq[Double]): SD =
	{
		val sd = Array.fill(vectors.head.size)(0D)
		vectors.foreach( v => v.seq.indices.foreach( i => sd(i) = sd(i) + pow(v(i) - mean(i), 2) ) )
		sd.map(_ / (vectors.size - 1))
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

	def obtainMinAndMax[S <: Seq[Double]](data: GenSeq[S]): (MB[Double], MB[Double]) =
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

	def obtainCenterFollowingWeightedDistribution[V](distribution: mutable.Buffer[(V, Double)]): V =
	{
		val p = scala.util.Random.nextDouble * distribution.map(_._2).sum
		var cpt = 0
		var accum = 0D
		while ( accum <= p ) {
			accum += distribution(cpt)._2
			cpt += 1
		}
		distribution(cpt - 1)._1
	}
}