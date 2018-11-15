package org.clustering4ever.scala.clusterizables
/**
 * @author Beck Gaël
 */
import scala.collection.immutable
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.scala.vectorizables.{Vectorizable, Vector, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
import org.clustering4ever.clustering.ClusteringArgs
/**
 * Trait defining methods that returns a new object with vector appended
 */
trait ComplementaryVectors[V, Self <: ComplementaryVectors[V, Self]] {
	/**
	 *
	 */
	def addAltVector(idx: Int, altVector: V): Self
	/**
	 *
	 */
	val altVectors: immutable.HashMap[Int, V]
}
/**
 * Identified Object
 */
abstract class IdentifiedVectorizable[@specialized(Int, Long) ID: Numeric, O, V](
	val id: ID,
	val vectorizable: Vectorizable[O, V]
) extends Serializable {
	/**
	 *
	 */
	final lazy val originalVector: V = vectorizable.toVector
	/**
	 *
	 */
	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + id.hashCode
		result
	}
}
/**
 *
 */
case class IdentifiedVector[ID: Numeric, V](
	override val id: ID,
	override val vectorizable: Vector[V],
	override val altVectors: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]
) extends IdentifiedVectorizable[ID, V, V](id, vectorizable) with ComplementaryVectors[V, IdentifiedVector[ID, V]] {
	/**
	 *
	 */
	def addAltVector(idx: Int, altVector: V) = this.copy(altVectors = if( ! altVectors.isEmpty ) altVectors + ((idx, altVector)) else immutable.HashMap(idx -> altVector))
	/**
	 *
	 */
	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + id.hashCode
		result = prime * result + altVectors.hashCode
		result
	}
}
/**
 *
 */
abstract class Clusterizable[ID: Numeric, O, V, Self <: Clusterizable[ID, O, V, Self]](
	id: ID, 
	vectorizable: Vectorizable[O, V],
	val clusterID: Option[Int] = None
) extends IdentifiedVectorizable[ID, O, V](id, vectorizable) {
	/**
	 *
	 */
	def vector(implicit workingVector: Int = 0): V = originalVector
	/**
	 *
	 */
	def addClusterID(newCID: Int): Self
	/**
	 *
	 */
	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + id.hashCode
		result = prime * result + originalVector.hashCode
		result = prime * result + clusterID.hashCode
		result
	}

}
/**
 *
 */
abstract class ClusterizableExt[ID: Numeric, O, V, Self <: ClusterizableExt[ID, O, V, Self]](
	id: ID, 
	vectorizable: Vectorizable[O, V],
	clusterID: Option[Int] = None,
	val altVectors: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]
) extends Clusterizable[ID, O, V, Self](id, vectorizable, clusterID) with ComplementaryVectors[V, Self] {
	/**
	 *
	 */
	override def vector(implicit workingVector: Int = 0): V = if( workingVector == 0 ) originalVector else altVectors(workingVector)
	/**
	 *
	 */
	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + id.hashCode
		result = prime * result + originalVector.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + altVectors.hashCode
		result
	}

}
/**
 *
 **/
case class EasyClusterizable[ID: Numeric, O, V](
	override val id: ID,
	override val vectorizable: Vectorizable[O, V],
	override val clusterID: Option[Int] = None
) extends Clusterizable[ID, O, V, EasyClusterizable[ID, O, V]](id, vectorizable, clusterID) {
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
	def addClusterID(newCID: Int): EasyClusterizable[ID, O, V] = this.copy(clusterID = Some(newCID))
}
/**
 *
 **/
case class EasyClusterizableExt[ID: Numeric, O, V](
	override val id: ID,
	override val vectorizable: Vectorizable[O, V],
	override val clusterID: Option[Int] = None,
	override val altVectors: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]
) extends ClusterizableExt[ID, O, V, EasyClusterizableExt[ID, O, V]](id, vectorizable, clusterID, altVectors/*, altClusterIDs*/) {
	/**
	 *
	 */
	override def canEqual(a: Any): Boolean = a.isInstanceOf[EasyClusterizableExt[ID, O, V]]
	/**
	 *
	 */
	override def equals(that: Any): Boolean = {
		that match {
		  case that: EasyClusterizableExt[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	/**
	 *
	 */
	def addClusterID(newCID: Int): EasyClusterizableExt[ID, O, V] = this.copy(clusterID = Some(newCID))
	/**
	 *
	 */
	def addAltVector(idx: Int, altVector: V): EasyClusterizableExt[ID, O, V] = this.copy(altVectors = if( ! altVectors.isEmpty ) altVectors + ((idx, altVector)) else immutable.HashMap(idx -> altVector))
}