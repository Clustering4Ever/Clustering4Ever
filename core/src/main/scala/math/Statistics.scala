package org.clustering4ever.stats
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{sqrt, pow, min, max, Pi}
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.util.SumVectors
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clustering.ClusteringSharedTypes
/**
 *
 */
object Stats extends ClusteringSharedTypes {
	/**
	 * @return the standard deviation between vectors and a mean
	 */
	final def sd(vectors: GenSeq[mutable.ArrayBuffer[Double]], mean: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
		val sd = mutable.ArrayBuffer.fill(vectors.head.size)(0D)
		vectors.foreach( v => v.indices.foreach{ i => 
			val toPow2 = v(i) - mean(i)
			sd(i) = sd(i) + toPow2 * toPow2
		})
		sd.map( v => sqrt(v / (vectors.size - 1)) )
	}
	/**
	 * @return min and max for the ith component in reduce style
	 */
	final def obtainIthMinMax[V <: Seq[Double]](idx: Int, vminMax1: (V, V), vminMax2: (V, V)): (Double, Double) = {
		(
			min(vminMax1._1(idx), vminMax2._1(idx)),
			max(vminMax1._2(idx), vminMax2._2(idx))
		)
	}
	/**
	 *
	 */
	final def obtainMinAndMax[V <: Seq[Double]](data: GenSeq[V]): (V, V) = {
		val dim = data.head.size
		val range = (0 until dim)
		val (minValues, maxValues) = data.map( v => (v, v) ).reduce{ (minMaxa, minMaxb) =>
			val builderMin = data.head.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, V]]
			val builderMax = data.head.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, V]]
			builderMin.sizeHint(dim)
			builderMax.sizeHint(dim)
			range.foreach{ i => 
				val (min, max) = obtainIthMinMax(i, minMaxa, minMaxb)
				builderMin += min
				builderMax += max
			}
			(builderMin.result, builderMax.result)
		}
		(minValues, maxValues)
	}
	/**
	 *
	 */
	final def obtainMedianFollowingWeightedDistribution[V](distribution: Seq[(V, Double)]): V = {
		val p = scala.util.Random.nextDouble * distribution.map(_._2).sum
		@annotation.tailrec
		def go(accum: Double, i: Int): Int = {
			if(accum < p) go(accum + distribution(i)._2, i + 1)
			else i
		}
		val cpt = go(0D, 0)
		if(cpt == 0) distribution.head._1 else distribution(cpt - 1)._1
	}
}