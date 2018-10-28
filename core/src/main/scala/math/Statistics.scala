package clustering4ever.stats
/**
 * @author Beck Gaël
 */
import scala.math.{sqrt, pow, min, max, Pi}
import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.util.SumVectors
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringCommons
import clustering4ever.util.CommonTypes

object Stats extends ClusteringCommons with CommonTypes {
	/**
	 * @return the standard deviation between vectors and a mean
	 */
	def sd(vectors: GenSeq[mutable.ArrayBuffer[Double]], mean: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
		val sd = mutable.ArrayBuffer.fill(vectors.head.size)(0D)
		vectors.foreach( v => v.seq.indices.foreach{ i => 
			val toPow2 = v(i) - mean(i)
			sd(i) = sd(i) + toPow2 * toPow2
		})
		sd.map( v => sqrt(v / (vectors.size - 1)) )
	}
	/**
	 * @return min and max for the ith component in reduce style
	 */
	def obtainIthMinMax(idx: Int, vminMax1: (MB[Double], MB[Double]), vminMax2: (MB[Double], MB[Double])) = {
		(
			min(vminMax1._1(idx), vminMax2._1(idx)),
			max(vminMax1._2(idx), vminMax2._2(idx))
		)
	}
	/**
	 *
	 */
	def obtainMinAndMax[S <: Seq[Double]](data: GenSeq[S]): (MB[Double], MB[Double]) = {
		val dim = data.head.size
		val vectorRange = (0 until dim).toBuffer
		val (minValues, maxValues) = data.map{ v => 
			val buff = v.toBuffer
			(buff, buff) 
		}.reduce( (minMaxa, minMaxb) => vectorRange.map( i => obtainIthMinMax(i, minMaxa, minMaxb) ).unzip )
		(minValues, maxValues)
	}
	/**
	 *
	 */
	def obtainCenterFollowingWeightedDistribution[V](distribution: mutable.Buffer[(V, Double)]): V = {
		val p = scala.util.Random.nextDouble * distribution.map(_._2).sum
		var cpt = 0
		var accum = 0D
		while ( accum < p ) {
			accum += distribution(cpt)._2
			cpt += 1
		}
		if( cpt == 0 ) distribution.head._1 else distribution(cpt - 1)._1
	}
}