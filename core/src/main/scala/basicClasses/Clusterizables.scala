package org.clustering4ever.scala.clusterizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable}
import spire.math.{Numeric => SNumeric}
import shapeless.HMap
import org.clustering4ever.shapelesslinked.VMapping
import org.clustering4ever.scala.vectorizables.Vectorizable
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.scala.vectors.{GVector, BinaryVector, ScalarVector, MixtVector}
/**
 * Identified Raw Object
 */
trait IdentifiedRawObject[ID, O] extends Serializable {
	/**
	 *
	 */
	val id: ID
	/**
	 *
	 */
	val vectorizable: Vectorizable[O]
	/**
	 *
	 */
	override def hashCode(): Int = id.hashCode
	/**
	 *
	 */
	// def toEasyClusterizable[V <: GVector](towardVector: (Int, O => V)) = new EasyClusterizable(id, vectorizable, vectorizable.toVector(towardVector._2))
}
/**
 *
 */
trait IdentifiedVectorGen[ID, O, V] extends IdentifiedRawObject[ID, O] {
	/**
	 *
	 */
	val workingVector: V
}
/**
 *
 */
trait IdentifiedGVector[ID, O, V <: GVector] extends IdentifiedVectorGen[ID, O, V]
/**
 *
 */
trait IdentifiedVectorized[ID, O, V <: GVector] extends IdentifiedGVector[ID, O, V] {
	/**
	 *
	 */
	val vectorized: HMap[VMapping]
}
/**
 *
 */
case class EasyIdentifiedVector[ID, O, V <: GVector](
	val id: ID,
	val vectorizable: Vectorizable[O],
	val workingVector: V,
	val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends IdentifiedVectorized[ID, O, V]
/**
 *
 */
trait Clusterizable[ID, O, V <: GVector, Self[A, B, C <: GVector] <: Clusterizable[A, B, C, Self]] extends IdentifiedVectorized[ID, O, V] {
	/**
	 *
	 */
	val clusterID: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int]
	/**
	 *
	 */
	def addClusterID(newClusterID: Int): Self[ID, O, V]
	/**
	 *
	 */
	def addVectorized[GV <: GVector](vectorizationID: Int, towardNewVector: O => GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self[ID, O, V]
	/**
	 *
	 */
	def addAltVector[GV <: GVector](vectorizationID: Int, newAltVector: GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self[ID, O, V]
	/**
	 *
	 */
	def updtWorkingVector[GV <: GVector](vectorizationID: Int)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self[ID, O, GV]
}
/**
 *
 */
case class EasyClusterizable[ID, O, V <: GVector](
	val id: ID,
	val vectorizable: Vectorizable[O],
	val workingVector: V,
	val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends Clusterizable[ID, O, V, EasyClusterizable] {
	/**
	 *
	 */
	override def canEqual(a: Any): Boolean = a.isInstanceOf[EasyClusterizable[ID, O, V]]
	/**
	 *
	 */
	override def equals(that: Any): Boolean = {
		that match {
		  case that: EasyClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	/**
	 *
	 */
	def addClusterID(newClusterID: Int): EasyClusterizable[ID, O, V] = {
		clusterID += newClusterID
		this
	}
	/**
	 *
	 */
	def addVectorized[GV <: GVector](vectorizationID: Int, towardNewVector: O => GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasyClusterizable[ID, O, V] = {
		this.copy(vectorized = vectorized + ((vectorizationID, vectorizable.toVector(towardNewVector))))
	}
	/**
	 *
	 */
	def addAltVector[GV <: GVector](vectorizationID: Int, newAltVector: GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasyClusterizable[ID, O, V] = {
		this.copy(vectorized = vectorized + ((vectorizationID, newAltVector)))
	}
	/**
	 *
	 */
	def updtWorkingVector[GV <: GVector](vectorizationID: Int)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasyClusterizable[ID, O, GV] = {
		new EasyClusterizable(id, vectorizable, vectorized.get(vectorizationID).get.asInstanceOf[GV], vectorized)
	}
}