package org.clustering4ever.math.distances.mixt
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{pow, sqrt}
import org.clustering4ever.math.distances.MixtDistance
import org.clustering4ever.vectors.MixtVector
import org.clustering4ever.clusterizables.Clusterizable
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
	protected def hammingAndEuclidean(dot1: MixtVector[Vb, Vs], dot2: MixtVector[Vb, Vs]): Double = {

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
class HammingAndEuclidean[Vb <: Seq[Int], Vs <: Seq[Double]](val alpha: Double = 0D) extends HammingAndEuclideanMeta[Vb, Vs] with MixtDistance[Vb, Vs] {
	/**
	 *	
	 */
	def d(dot1: MixtVector[Vb, Vs], dot2: MixtVector[Vb, Vs]): Double = hammingAndEuclidean(dot1, dot2)	
}
/**
 *
 */
// class HammingAndEuclideanClusterizable[Vb <: Seq[Int], Vs <: Seq[Double], D <: HammingAndEuclidean[Vb, Vs]](val alpha: Double = 0D, val classicalMetric: D, workingVector: Int = 0) extends HammingAndEuclideanMeta[Vb, Vs] with MixtClusterizableDistance[Vb, Vs, D] {
// 	/**
// 	 *
// 	 */
// 	def d[@specialized(Int, Long) ID, O, Cz[X, Y, Z] <: Clusterizable[X, Y, Z, Cz[X, Y, Z]]](dot1: Cz[ID, O, MixtVector[Vb, Vs]], dot2: Cz[ID, O, MixtVector[Vb, Vs]]): Double = hammingAndEuclidean(dot1.vector(workingVector), dot2.vector(workingVector))
// }