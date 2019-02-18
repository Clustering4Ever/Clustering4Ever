package org.clustering4ever.math.distances.mixt
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{pow, sqrt}
import org.clustering4ever.math.distances.MixedDistance
import org.clustering4ever.vectors.MixedVector
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.math.distances.scalar.EuclideanAncestor
import org.clustering4ever.math.distances.binary.HammingMeta
/**
 *
 */
trait HammingAndEuclideanMeta[Vb <: Seq[Int], Vs <: Seq[Double]] extends EuclideanAncestor[Vs] with HammingMeta[Vb] {
	/**
	 * The α coefficient to combined results from scalar part and binary part
	 *  - if different from 0 it took the form of HammingDistance + α * EuclideanDistance
	 *  - If equals 0 it took the form of (HammingDistance * (BinaryFeaturesNumber / totalFeatures)) + (EuclideanDistance * (ScalarFeaturesNumber / totalFeatures))
	 */
	val alpha: Double
	/**
	 *
	 */
	protected final def hammingAndEuclidean(v1: (Vb, Vs), v2: (Vb, Vs)): Double = {
		val ds = euclidean(v1._2, v2._2)
		val db = hamming(v1._1, v2._1)
		if(alpha == 0) {
			val nbDim = v1._1.size + v1._2.size
			db * (v1._1.size.toDouble / nbDim) + ds * (v1._2.size.toDouble / nbDim)
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
final case class HammingAndEuclidean[Vb <: Seq[Int], Vs <: Seq[Double]](final val alpha: Double = 0D, final val squareRoot: Boolean = true, final val id: MetricID = 10) extends HammingAndEuclideanMeta[Vb, Vs] with MixedDistance[Vb, Vs] {
	/**
	 *	
	 */
	final def d(v1: (Vb, Vs), v2: (Vb, Vs)): Double = hammingAndEuclidean(v1, v2)	
	/**
	 *	
	 */
	final def d(v1: MixedVector[Vb, Vs], v2: MixedVector[Vb, Vs]): Double = {
		val MixedVector(binary1, scalar1) = v1
		val MixedVector(binary2, scalar2) = v2
		hammingAndEuclidean((binary1, scalar1), (binary2, scalar2))
	}
}