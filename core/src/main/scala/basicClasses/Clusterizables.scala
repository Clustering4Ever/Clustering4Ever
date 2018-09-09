package clustering4ever.scala.clusterizables

/**
 * @author Beck Gaël
 **/

import clustering4ever.scala.vectorizables.{Vectorizable, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
abstract class Clusterizable[ID: Numeric, Vector](val id: ID, val vectorizable: Vectorizable[Vector]) extends Serializable {
	final lazy val vector: Vector = vectorizable.toVector
}
/**
 *
 */
abstract class ClusterizableExt[ID: Numeric, Vector](
	id: ID, 
	vectorizable: Vectorizable[Vector]
) extends Clusterizable[ID, Vector](id, vectorizable) {

	var v2: Option[Vector]
	var clusterID: Option[Int]
	
	def setV2[T <: ClusterizableExt[ID, Vector]](newV2: Vector): T = {
		v2 = Some(newV2)
		this.asInstanceOf[T]
	}

	def setClusterID[T <: ClusterizableExt[ID, Vector]](newCID: Int): T = {
		clusterID = Some(newCID)
		this.asInstanceOf[T]
	}
}
/**
 *
 */
abstract class ClusterizableExtVectors[ID: Numeric, T: Numeric, Vector <: Seq[T]](
	id: ID, 
	vectorizable: Vectorizable[Vector]
) extends ClusterizableExt[ID, Vector](id, vectorizable) {

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
 * Generic clusterizable for both Mixt Vectors => (Vector[Int], Vector[Double]) 
 **/
case class MixtClusterizable[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](
	override val id: ID,
	override val vectorizable: MixtVectorizable[Obj, Vb, Vs, V],
	var v2: Option[V] = None,
	var clusterID: Option[Int] = None
) extends ClusterizableExtMixt[ID, Vb, Vs, V](id, vectorizable) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[MixtClusterizable[ID, Obj, Vb, Vs, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: MixtClusterizable[ID, Obj, Vb, Vs, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def copy() = new MixtClusterizable[ID, Obj, Vb, Vs, V](id, vectorizable.asInstanceOf[MixtVectorizable[Obj, Vb, Vs, V]])
}
/**
 * Clusterizable for Vector[Double] 
 **/
case class RealClusterizable[ID: Numeric, Obj, V <: Seq[Double]](
	idTmp: ID,
	override val vectorizable: RealVectorizable[Obj, V],
	var v2: Option[V] = None,
	var clusterID: Option[Int] = None
) extends ClusterizableExtVectors[ID, Double, V](idTmp, vectorizable) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[RealClusterizable[ID, Obj, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: RealClusterizable[ID, Obj, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	def copy() = new RealClusterizable[ID, Obj, V](id, vectorizable.asInstanceOf[RealVectorizable[Obj, V]], v2, clusterID)
}
/**
 * Clusterizable for Vector[Int] 
 **/
case class BinaryClusterizable[ID: Numeric, Obj, V <: Seq[Int]](
	idTmp: ID,
	override val vectorizable: BinaryVectorizable[Obj, V],
	var v2: Option[V] = None,
	var clusterID: Option[Int] = None
) extends ClusterizableExtVectors[ID, Int, V](idTmp, vectorizable) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[BinaryClusterizable[ID, Obj, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: BinaryClusterizable[ID, Obj, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def copy() = new BinaryClusterizable[ID, Obj, V](id, vectorizable.asInstanceOf[BinaryVectorizable[Obj, V]], v2, clusterID)
}