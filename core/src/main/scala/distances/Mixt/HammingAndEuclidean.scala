package clustering4ever.math.distances.mixt

import scala.math.{pow, sqrt}
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.math.distances.MixtDistanceClusterizable
import clustering4ever.scala.clusterizables.MixtClusterizable

/**
 * @author Beck Gaël
 */
trait HammingAndEuclideanMeta[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Serializable {
	protected val α: Double

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

		if( α == 0 ) {
			val nbDim = dot1.binary.size + dot1.scalar.size
			db * (dot1.binary.size.toDouble / nbDim) + ds * (dot1.scalar.size.toDouble / nbDim) 
		}
		else db + α * ds
	}
}

class HammingAndEuclidean[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](val α: Double = 0D) extends HammingAndEuclideanMeta[Vb, Vs, V] with MixtDistance[Vb, Vs, V] {
	/**
	 *	
	 */
	override def d(dot1: V, dot2: V): Double = hammingAndEuclidean(dot1, dot2)
	
}

class HammingAndEuclideanClusterizable[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](val α: Double = 0D, val classicalMetric: HammingAndEuclidean[Vb, Vs, V]) extends HammingAndEuclideanMeta[Vb, Vs, V] with MixtDistanceClusterizable[ID, Obj, Vb, Vs, V] {
	/**
	 *	
	 */
	override def d(dot1: MixtClusterizable[ID, Obj, Vb, Vs, V], dot2: MixtClusterizable[ID, Obj, Vb, Vs, V]): Double = hammingAndEuclidean(dot1.vector, dot2.vector)
}