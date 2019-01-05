package org.clustering4ever.clusterizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import shapeless.HMap
import org.clustering4ever.identifiables.IdentifiedVectorized
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.scala.vectorizables.Vectorizable
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.scala.vectors.GVector
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.preprocessables.Preprocessable
/**
 * Basic trait for Clusterizable objects
 */
trait Clusterizable[ID, O, V <: GVector[V], Self[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Self]] extends Preprocessable[ID, O, V, Self] {
	/**
	 *
	 * Replace Or Not By a immutable.Vector in order to keep immutability ???
	 */
	val clusterIDs: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int]
	/**
	 *
	 */
	def addClusterID(newClusterID: Int): Self[ID, O, V]
}
/**
 *
 */
case class EasyClusterizable[ID, O, V <: GVector[V]](
	val id: ID,
	val o: Vectorizable[O],
	val v: V,
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
		  case that: EasyClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode && that.hashCode2 == this.hashCode2
		  case _ => false
		}
	}
	/**
	 *
	 */
	def addClusterID(newClusterID: Int): EasyClusterizable[ID, O, V] = {
		clusterIDs += newClusterID
		this
	}
	/**
	 *
	 */
	def addVectorized[GV <: GVector[GV]](vectorizationID: Int, towardNewVector: O => GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasyClusterizable[ID, O, V] = {
		this.copy(vectorized = vectorized + ((vectorizationID, o.toVector(towardNewVector))))
	}
	/**
	 *
	 */
	def addAltVector[GV <: GVector[GV]](vectorizationID: Int, newAltVector: GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasyClusterizable[ID, O, V] = {
		this.copy(vectorized = vectorized + ((vectorizationID, newAltVector)))
	}
	/**
	 *
	 */
	def updtV[GV <: GVector[GV]](vectorizationID: Int)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasyClusterizable[ID, O, GV] = {
		new EasyClusterizable(id, o, vectorized.get(vectorizationID).get.asInstanceOf[GV], vectorized)
	}
}