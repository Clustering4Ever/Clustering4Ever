package org.clustering4ever.distances.mixt

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.MixedDistance
import org.clustering4ever.distances.binary.HammingMeta
import org.clustering4ever.distances.continuous.EuclideanAncestor
import org.clustering4ever.roottraits.MetricIDType._
import org.clustering4ever.roottraits.MixedVector

import scala.language.higherKinds
/**
 *
 */
trait HammingAndEuclideanMeta extends EuclideanAncestor with HammingMeta {
	/**
	 * The α coefficient to combined results from scalar part and binary part
	 *  - if different from 0 it took the form of HammingDistance + α * EuclideanDistance
	 *  - If equals 0 it took the form of (HammingDistance * (BinaryFeaturesNumber / totalFeatures)) + (EuclideanDistance * (ScalarFeaturesNumber / totalFeatures))
	 */
	val alpha: Double
	/**
	 *
	 */
	protected final def hammingAndEuclidean(v1: (Array[Int], Array[Double]), v2: (Array[Int], Array[Double])): Double = {
		val ds = euclidean(v1._2, v2._2)
		val db = hamming(v1._1, v2._1)
		if(alpha == 0) {
			val nbDim = v1._1.length + v1._2.length
			db * (v1._1.length.toDouble / nbDim) + ds * (v1._2.length.toDouble / nbDim)
		}
		else db + alpha * ds
	}
}
/**
 * @param alpha The α coefficient to combined results from scalar part and binary part
 *  - if different from 0 it took the form of HammingDistance + α * EuclideanDistance
 *  - If equals 0 it took the form of (HammingDistance * (BinaryFeaturesNumber / totalFeatures)) + (EuclideanDistance * (ScalarFeaturesNumber / totalFeatures))
 * @param squareRoot if true applied the squared root to euclidean distance
 * @param id the unique identifier of this metric
 */
final case class HammingAndEuclidean(final val alpha: Double = 0D, final val squareRoot: Boolean = true, final val id: MetricID = 10) extends HammingAndEuclideanMeta with MixedDistance {
	/**
	 *	
	 */
	final def dRaw(v1: (Array[Int], Array[Double]), v2: (Array[Int], Array[Double])): Double = hammingAndEuclidean(v1, v2)	
	/**
	 *	
	 */
	final def d(v1: MixedVector, v2: MixedVector): Double = {
		val MixedVector(binary1, scalar1) = v1
		val MixedVector(binary2, scalar2) = v2
		hammingAndEuclidean((binary1, scalar1), (binary2, scalar2))
	}
}