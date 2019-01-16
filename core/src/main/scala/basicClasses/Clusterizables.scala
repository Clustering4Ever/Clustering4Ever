package org.clustering4ever.clusterizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable}
import shapeless.HMap
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.vectorizables.{Vectorizable, NotVectorizable, VectorizableOrNot}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorizationLocal}

/**
 * Basic trait for Clusterizable objects
 */
trait Clusterizable[ID, O, V <: GVector[V], Self[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Self]] extends Preprocessable[ID, O, V, Self] {
	/**
	 *
	 */
	val clusterIDs: immutable.Vector[Int]
	/**
	 *
	 */
	def addClusterIDs(newClusterIDs: Int*): Self[ID, O, V]
	/**
	 *
	 */
	def overwriteClusterIDs(newClusterIDs: Int*): Self[ID, O, V]	 
}
/**
 * EasyClusterizable facilitator
 */
object EasyClusterizable {	
	/**
	 * Simplest way to generate an EasyClusterizable
	 */
	def apply[ID, V <: GVector[V]](id: ID, v: V): EasyClusterizable[ID, V, V] = new EasyClusterizable(id, Vectorizable(v), v, HMap[VMapping](0 -> v)(VMapping[Int, V]))
	/**
	 * Generate a proper EasyClusterizable
	 */
	def apply[ID, O, V <: GVector[V]](id: ID, o: O, v: V): EasyClusterizable[ID, O, V] = new EasyClusterizable(id, Vectorizable(o), v, HMap[VMapping](0 -> v)(VMapping[Int, V]))
	/**
	 *
	 */
	def apply[ID, O, V <: GVector[V]](id: ID, o: O, towardVector: O => V) = {
		val vecto = Vectorizable(o)
		val v = vecto.toVector(towardVector)
		new EasyClusterizable(id, vecto, v, HMap[VMapping](0 -> v)(VMapping[Int, V]))
	}
	/**
	 * Simplest way to generate an EasyClusterizable for real vectors
	 */
	def rawApplyScalar[ID, V <: Seq[Double]](id: ID, v: V) = apply(id, ScalarVector(v))
	/**
	 * Simplest way to generate an EasyClusterizable for binary vectors
	 */
	def rawApplyBinary[ID, V <: Seq[Int]](id: ID, v: V) = apply(id, BinaryVector(v))
	/**
	 * Simplest way to generate an EasyClusterizable for mixt vectors
	 */
	def rawApplyMixt[ID, Vb <: Seq[Int], Vs <: Seq[Double]](id: ID, binary: Vb, scalar: Vs) = apply(id, MixtVector(binary, scalar))
}
/**
 *
 */
case class EasyClusterizable[ID, O, V <: GVector[V]](
	val id: ID,
	val o: Vectorizable[O],
	val v: V,
	val vectorized: HMap[VMapping] = HMap.empty[VMapping],
	val clusterIDs: immutable.Vector[Int] = immutable.Vector.empty[Int]
) extends Clusterizable[ID, O, V, EasyClusterizable] {
	/**
	 *
	 */
	final override def canEqual(a: Any): Boolean = a.isInstanceOf[EasyClusterizable[ID, O, V]]
	/**
	 *
	 */
	final override def equals(that: Any): Boolean = {
		that match {
		  case that: EasyClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode && that.hashCode2 == this.hashCode2
		  case _ => false
		}
	}
	/**
	 *
	 */
	final def addClusterIDs(newClusterIDs: Int*): EasyClusterizable[ID, O, V] = {
		this.copy(clusterIDs = clusterIDs ++ newClusterIDs)
	}
	/**
	 *
	 */
	final def overwriteClusterIDs(newClusterIDs: Int*): EasyClusterizable[ID, O, V] = {
		this.copy(clusterIDs = immutable.Vector(newClusterIDs:_*))
	}
	/**
	 *
	 */
	final def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): EasyClusterizable[ID, O, V] = {
		this.copy(vectorized = vectorized.+(((vectorization.vectorizationID, o.toVector(vectorization.vectorizationFct.get))))(VMapping[VectorizationID, GV]))
	}
	/**
	 *
	 */
	final def addAlternativeVector[GV <: GVector[GV]](vectorizationID: VectorizationID, newAlternativeVector: GV): EasyClusterizable[ID, O, V] = {
		this.copy(vectorized = vectorized.+(((vectorizationID, newAlternativeVector)))(VMapping[VectorizationID, GV]))
	}
	/**
	 *
	 */
	final def switchForExistingVector[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): EasyClusterizable[ID, O, GV] = {
		this.copy(v = vectorized.get(vectorization.vectorizationID)(vectorization.vMapping).get)
	}
	/**
	 *
	 */
	final def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): EasyClusterizable[ID, O, GV] = {
		this.copy(v = o.toVector(vectorization.vectorizationFct.get))
	}

}