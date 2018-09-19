package clustering4ever.scala.clusterizables
/**
 * @author Beck Gaël
 */
import clustering4ever.scala.vectorizables.{Vectorizable, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import spire.math.{Numeric => SNumeric}
/**
 *
 */
abstract class Clusterizable[ID: Numeric, V](val id: ID, val vectorizable: Vectorizable[V]) extends Serializable {
	final lazy val vector: V = vectorizable.toVector
}
/**
 *
 */
abstract class ClusterizableExt[ID: Numeric, V](
	id: ID, 
	vectorizable: Vectorizable[V]
) extends Clusterizable[ID, V](id, vectorizable) {

	var v2: Option[V]
	var clusterID: Option[Int]
	
	def setV2(newV2: V): this.type = {
		v2 = Some(newV2)
		this
	}

	def setClusterID(newCID: Int): this.type = {
		clusterID = Some(newCID)
		this
	}
}
/**
 *
 */
abstract class ClusterizableExtVectors[ID: Numeric, @specialized(Int, Double) N: SNumeric, V <: Seq[N]](
	id: ID, 
	vectorizable: Vectorizable[V]
) extends ClusterizableExt[ID, V](id, vectorizable) {

	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + vector.hashCode
		result = prime * result + v2.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + id.hashCode
		result
	}
}
/**
 *
 */
abstract class ClusterizableExtMixt[ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double], Vectors <: BinaryScalarVector[Vb, Vs]](
	id: ID, 
	vectorizable: Vectorizable[Vectors]
) extends ClusterizableExt[ID, Vectors](id, vectorizable) {

	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + vector.binary.hashCode
		result = prime * result + vector.scalar.hashCode
		result = prime * result + v2.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + id.hashCode
		result
	}
}
/**
 * Generic clusterizable for both Mixt Vectors => (V[Int], V[Double]) 
 */
case class MixtClusterizable[ID: Numeric, O, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](
	override val id: ID,
	override val vectorizable: MixtVectorizable[O, Vb, Vs, V],
	var v2: Option[V] = None,
	var clusterID: Option[Int] = None
) extends ClusterizableExtMixt[ID, Vb, Vs, V](id, vectorizable) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[MixtClusterizable[ID, O, Vb, Vs, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: MixtClusterizable[ID, O, Vb, Vs, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def copy() = new MixtClusterizable[ID, O, Vb, Vs, V](id, vectorizable.asInstanceOf[MixtVectorizable[O, Vb, Vs, V]])
}
/**
 * Clusterizable for V[Double] 
 */
case class RealClusterizable[ID: Numeric, O, V <: Seq[Double]](
	override val id: ID,
	override val vectorizable: RealVectorizable[O, V],
	var v2: Option[V] = None,
	var clusterID: Option[Int] = None
) extends ClusterizableExtVectors[ID, Double, V](id, vectorizable) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[RealClusterizable[ID, O, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: RealClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	def copy() = new RealClusterizable[ID, O, V](id, vectorizable.asInstanceOf[RealVectorizable[O, V]], v2, clusterID)
}
/**
 * Clusterizable for V[Int] 
 */
case class BinaryClusterizable[ID: Numeric, O, V <: Seq[Int]](
	override val id: ID,
	override val vectorizable: BinaryVectorizable[O, V],
	var v2: Option[V] = None,
	var clusterID: Option[Int] = None
) extends ClusterizableExtVectors[ID, Int, V](id, vectorizable) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[BinaryClusterizable[ID, O, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: BinaryClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def copy() = new BinaryClusterizable[ID, O, V](id, vectorizable.asInstanceOf[BinaryVectorizable[O, V]], v2, clusterID)
}