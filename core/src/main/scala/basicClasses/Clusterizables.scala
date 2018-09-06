package clustering4ever.scala.clusterizables

import clustering4ever.scala.vectorizables.{Vectorizable, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector

abstract class Clusterizable[ID: Numeric, Vector](final val id: ID, val vectorizable: Vectorizable[Vector]) extends Serializable {
	final lazy val vector: Vector = vectorizable.toVector
}

abstract class ClusterizableExt[ID: Numeric, Vector](
	id: ID, 
	vectorizable: Vectorizable[Vector],
	var v2: Vector,
	var clusterID: Int = Int.MaxValue
) extends Clusterizable[ID, Vector](id, vectorizable) {

	def setV2(newV2: Vector): this.type

	def setClusterID(newCID: Int): this.type
}

abstract class ClusterizableExtVectors[ID: Numeric, T: Numeric, Vector <: Seq[T]](
	id: ID, 
	vectorizable: Vectorizable[Vector],
	v2: Vector,
	clusterID: Int = Int.MaxValue
) extends ClusterizableExt[ID, Vector](id, vectorizable, v2, clusterID) {

	@transient lazy val v1Seq = vector.toSeq
	@transient lazy val v2Seq = v2.toSeq

	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + v1Seq.hashCode
		result = prime * result + v2Seq.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + id.hashCode
		result
	}
}

abstract class ClusterizableExtMixt[ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double], Vectors <: BinaryScalarVector[Vb, Vs]](
	id: ID, 
	vectorizable: Vectorizable[Vectors],
	v2: Vectors,
	clusterID: Int = Int.MaxValue
) extends ClusterizableExt[ID, Vectors](id, vectorizable, v2, clusterID) {

	@transient lazy val binarySeq = vector.binary
	@transient lazy val scalarSeq = vector.scalar

	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + binarySeq.hashCode
		result = prime * result + scalarSeq.hashCode
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
	idTmp: ID,
	vectorizableTmp: MixtVectorizable[Obj, Vb, Vs, V],
	v2Tmp: V = null.asInstanceOf[V],
	clusterIDTmp: Int = Int.MaxValue
) extends ClusterizableExtMixt[ID, Vb, Vs, V](idTmp, vectorizableTmp, v2Tmp, clusterIDTmp) {

	def setV2(newV2: V): this.type = {
		v2 = newV2
		this
	}

	def setClusterID(newCID: Int): this.type = {
		clusterID = newCID
		this
	}

	override def canEqual(a: Any): Boolean = a.isInstanceOf[MixtClusterizable[ID, Obj, Vb, Vs, V]]

	override def equals(that: Any): Boolean =
	{
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
	vectorizableTmp: RealVectorizable[Obj, V],
	v2Tmp: V = Seq.empty[Double].asInstanceOf[V],
	clusterIDTmp: Int = Int.MaxValue
) extends ClusterizableExtVectors[ID, Double, V](idTmp, vectorizableTmp, v2Tmp, clusterIDTmp) {

	def setV2(newV2: V): this.type = {
		v2 = newV2
		this
	}

	def setClusterID(newCID: Int): this.type = {
		clusterID = newCID
		this
	}

	override def canEqual(a: Any): Boolean = a.isInstanceOf[RealClusterizable[ID, Obj, V]]

	override def equals(that: Any): Boolean =
	{
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
	vectorizableTmp: BinaryVectorizable[Obj, V],
	v2Tmp: V = Seq.empty[Int].asInstanceOf[V],
	clusterIDTmp: Int = Int.MaxValue
) extends ClusterizableExtVectors[ID, Int, V](idTmp, vectorizableTmp, v2Tmp, clusterIDTmp) {

	def setV2(newV2: V): this.type = {
		v2 = newV2
		this
	}

	def setClusterID(newCID: Int): this.type = {
		clusterID = newCID
		this
	}

	override def canEqual(a: Any): Boolean = a.isInstanceOf[BinaryClusterizable[ID, Obj, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: BinaryClusterizable[ID, Obj, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def copy() = new BinaryClusterizable[ID, Obj, V](id, vectorizable.asInstanceOf[BinaryVectorizable[Obj, V]], v2, clusterID)
}