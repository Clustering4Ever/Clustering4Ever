package clustering4ever.math.distances.mixt
/**
 * @author Beck Gaël
 */
import scala.math.{pow, sqrt}
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.math.distances.MixtClusterizableDistance
import clustering4ever.scala.clusterizables.SimpleMixtClusterizable
/**
 *
 */
trait HammingAndEuclideanMeta[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Serializable {
	/**
	 *
	 */
	protected val alpha: Double
	/**
	 *
	 */
	protected def hammingAndEuclidean(dot1: V, dot2: V): Double = {
		var db = 0D
		var ds = 0D
		var i = 0
		while( i < dot1.scalar.size ) {
			val toPow2 = dot1.scalar(i) - dot2.scalar(i)
			ds += toPow2 * toPow2
			i += 1
		}

		ds = sqrt(ds)
		
		while( i < dot1.binary.size ) {
			db += dot1.binary(i) ^ dot2.binary(i)
			i += 1
		}

		if( alpha == 0 ) {
			val nbDim = dot1.binary.size + dot1.scalar.size
			db * (dot1.binary.size.toDouble / nbDim) + ds * (dot1.scalar.size.toDouble / nbDim) 
		}
		else db + alpha * ds
	}
}
/**
 *
 */
class HammingAndEuclidean[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](val alpha: Double = 0D) extends HammingAndEuclideanMeta[Vb, Vs, V] with MixtDistance[Vb, Vs, V] {
	/**
	 *	
	 */
	def d(dot1: V, dot2: V): Double = hammingAndEuclidean(dot1, dot2)
	
}
/**
 *
 */
class HammingAndEuclideanClusterizable[ID: Numeric, O, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs], D <: HammingAndEuclidean[Vb, Vs, V]](val alpha: Double = 0D, val classicalMetric: D) extends HammingAndEuclideanMeta[Vb, Vs, V] with MixtClusterizableDistance[SimpleMixtClusterizable[ID, O, Vb, Vs, V], Vb, Vs, V, D] {
	/**
	 *
	 */
	def d(dot1: SimpleMixtClusterizable[ID, O, Vb, Vs, V], dot2: SimpleMixtClusterizable[ID, O, Vb, Vs, V]): Double = hammingAndEuclidean(dot1.vector, dot2.vector)
}