package org.clustering4ever.clusterizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable}
import shapeless.HMap
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.clustering.ClusteringModel
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorizationLocal}
/**
 * Basic trait for Clusterizable objects
 * @tparam ID the ID corresponding to this clusterizable, it is recommend to put a type with an easy Ordering associate to it, ie Int, Long, Double
 * @tparam O the raw object from which vectorization are made
 * @tparam V the nature of the working vector on which algorithm are run
 * @tparam Self the concrete implementation of Clusterizable
 */
trait Clusterizable[O, V <: GVector[V], Self[B, C <: GVector[C]] <: Clusterizable[B, C, Self]] extends Preprocessable[O, V, Self] {

	this: Self[O, V] =>
	/**
	 * ClusterIDs in which belong this clusterizable, first clustering is at index 0, last one at index n - 1
	 */
	val clusterIDs: immutable.Vector[Int]
	/**
	 * add one or more clusterIDs existing clusterIDs of this clusterizable
	 * @param newClusterIDs : a Seq of new clusterIDs to add to this clusterizable
	 */
	def addClusterIDs(newClusterIDs: Int*): Self[O, V]
	/**
	 * Replace all clusterIDs by new ones
	 * @param newClusterIDs : a Seq of new clusterIDs to add to this clusterizable
	 */
	def overwriteClusterIDs(newClusterIDs: Int*): Self[O, V]	 
}
/**
 * EasyClusterizable facilitator
 */
object EasyClusterizable {	
	/**
	 * Simplest way to generate an EasyClusterizable
	 */
	final def apply[V <: GVector[V]](id: Long, v: V): EasyClusterizable[None.type, V] = EasyClusterizable(id, Vectorizable(None), v, HMap.empty[VMapping])
	/**
	 * Generate a proper EasyClusterizable
	 */
	final def apply[O, V <: GVector[V]](id: Long, o: O, v: V): EasyClusterizable[O, V] = EasyClusterizable(id, Vectorizable(o), v, HMap.empty[VMapping])
	/**
	 *
	 */
	final def apply[O, V <: GVector[V]](id: Long, o: O, towardVector: O => V): EasyClusterizable[O, V] = {
		val vectorizable = Vectorizable(o)
		val v = vectorizable.toVector(towardVector)
		EasyClusterizable(id, vectorizable, v, HMap.empty[VMapping])
	}
	/**
	 * Simplest way to generate an EasyClusterizable for real vectors
	 */
	final def rawApplyScalar[V <: Seq[Double]](id: Long, v: V) = apply(id, ScalarVector(v))
	/**
	 * Simplest way to generate an EasyClusterizable for binary vectors
	 */
	final def rawApplyBinary[V <: Seq[Int]](id: Long, v: V) = apply(id, BinaryVector(v))
	/**
	 * Simplest way to generate an EasyClusterizable for mixt vectors
	 */
	final def rawApplyMixed[Vb <: Seq[Int], Vs <: Seq[Double]](id: Long, binary: Vb, scalar: Vs) = apply(id, MixedVector(binary, scalar))
}
/**
 * A ready to work case class of Clusterizable trait for cluster without limits
 * @tparam ID the ID corresponding to this clusterizable, it is recommend to put a type with an easy Ordering associate to it, ie Int, Long, Double
 * @tparam O the raw object from which vectorization are made
 * @tparam V the nature of the working vector on which algorithm are run
 * @param id the ID of this clusterizable
 * @param o the raw object encapsulate inside a Vectorizable
 * @param v the working vector on which algorithm are run
 * @param vectorized the HMap containing various vectorization of the raw object o
 * @param clusterIDs the clustering indices for this clusterizable 
 */
final case class EasyClusterizable[O, V <: GVector[V]](
	final val id: Long,
	final val o: Vectorizable[O],
	final val v: V,
	final val vectorized: HMap[VMapping] = HMap.empty[VMapping],
	final val clusterIDs: immutable.Vector[Int] = immutable.Vector.empty[Int]
) extends Clusterizable[O, V, EasyClusterizable] {
	/**
	 *
	 */
	final override def canEqual(a: Any): Boolean = a.isInstanceOf[EasyClusterizable[O, V]]
	/**
	 *
	 */
	final override def equals(that: Any): Boolean = {
		that match {
		  case that: EasyClusterizable[O, V] => that.canEqual(this) && that.hashCode == this.hashCode && that.hashCode2 == this.hashCode2
		  case _ => false
		}
	}

	final def addClusterIDs(newClusterIDs: Int*): EasyClusterizable[O, V] = {
		this.copy(clusterIDs = clusterIDs ++ newClusterIDs)
	}

	final def overwriteClusterIDs(newClusterIDs: Int*): EasyClusterizable[O, V] = {
		this.copy(clusterIDs = immutable.Vector(newClusterIDs:_*))
	}

	final def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): EasyClusterizable[O, V] = {
		this.copy(vectorized = vectorized.+(((vectorization.vectorizationID, o.toVector(vectorization.vectorizationFct.get))))(VMapping[VectorizationID, GV]))
	}

	final def addAlternativeVector[GV <: GVector[GV]](vectorizationID: VectorizationID, alternativeVector: GV): EasyClusterizable[O, V] = {
		this.copy(vectorized = vectorized.+(((vectorizationID, alternativeVector)))(VMapping[VectorizationID, GV]))
	}

	final def switchForExistingVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): EasyClusterizable[O, GV] = {
		this.copy(v = vectorized.get(vectorization.vectorizationID)(vectorization.vMapping).get)
	}

	final def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): EasyClusterizable[O, GV] = {
		this.copy(v = o.toVector(vectorization.vectorizationFct.get))
	}

	final def updateVectorizationOfSameNature[Vecto <: Vectorization[O, V, Vecto]](vectorization: Vecto): EasyClusterizable[O, V] = {
		this.copy(v = o.toVector(vectorization.vectorizationFct.get))
	}

	final def updateWorkingVector[GV <: GVector[GV]](newWorkingVector: GV): EasyClusterizable[O, GV] = {
		EasyClusterizable(id, o, newWorkingVector, vectorized, clusterIDs)
	}

}