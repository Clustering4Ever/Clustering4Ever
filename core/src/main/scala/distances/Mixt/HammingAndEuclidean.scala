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
/**
 *
 */
trait HammingAndEuclideanMeta[Vb <: Seq[Int], Vs <: Seq[Double]] extends Serializable {
	/**
	 *
	 */
	protected val alpha: Double
	/**
	 *
	 */
	protected def hammingAndEuclidean(dot1: MixedVector[Vb, Vs], dot2: MixedVector[Vb, Vs]): Double = {

		@annotation.tailrec
		def goEuclidean(d: Double, i: Int): Double = {
			if(i < dot1.scalar.size) {
			  val toPow2 = dot1.scalar(i) - dot2.scalar(i)
			  goEuclidean(d + toPow2 * toPow2, i + 1)
			}
			else d
		}

		@annotation.tailrec
		def goHamming(d: Double, i: Int): Double = {
			if(i < dot1.binary.size) {
			  goHamming(d + (dot1.binary(i) ^ dot2.binary(i)), i + 1)
			}
			else d
		}

		val ds = sqrt(goEuclidean(0D, 0))
		val db = goHamming(0D, 0)

		if(alpha == 0) {
			val nbDim = dot1.binary.size + dot1.scalar.size
			db * (dot1.binary.size.toDouble / nbDim) + ds * (dot1.scalar.size.toDouble / nbDim) 
		}
		else db + alpha * ds
	}
}
/**
 *
 */
case class HammingAndEuclidean[Vb <: Seq[Int], Vs <: Seq[Double]](final val alpha: Double = 0D, final val id: MetricID = 10) extends HammingAndEuclideanMeta[Vb, Vs] with MixedDistance[Vb, Vs] {
	/**
	 *	
	 */
	def d(dot1: (Vb, Vs), dot2: (Vb, Vs)): Double = hammingAndEuclidean(MixedVector(dot1._1, dot1._2), MixedVector(dot2._1, dot2._2))	
	/**
	 *	
	 */
	def d(dot1: MixedVector[Vb, Vs], dot2: MixedVector[Vb, Vs]): Double = hammingAndEuclidean(dot1, dot2)	
}