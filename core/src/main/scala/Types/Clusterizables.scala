package clustering4ever.scala.clusterizables

import clustering4ever.scala.vectorizables.{Vectorizable, VectorizableM, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector

abstract class Clusterizable[ID: Numeric, Vector](val id: ID, val vectorizable: Vectorizable[Vector]) extends Serializable
{
	lazy val vector: Vector = vectorizable.toVector
}

abstract class ClusterizableExt[ID: Numeric, T: Numeric, Vector <: Seq[T]](
	id: ID, 
	vectorizable: Vectorizable[Vector],
	var v2: Vector,
	var clusterID: Int = Int.MaxValue
) extends Clusterizable[ID, Vector](id, vectorizable)
{
	@transient lazy val vectorSeq = vector.toSeq
	@transient lazy val vector2Seq = v2.toSeq

	def setV2(newV2: Vector): this.type

	def setClusterID(newCID: Int): this.type

	override def hashCode(): Int =
	{
		val prime = 31
		var result = 1
		result = prime * result + vectorSeq.hashCode
		result = prime * result + vector2Seq.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + id.hashCode
		result
	}
}

/**
 * Generic clusterizable for both Mixt Vectors => (Vector[Int], Vector[Double]) 
 **/
case class ClusterizableM[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double]](override val id: ID, override val vectorizable: VectorizableM[Obj, Vb, Vs]) extends Clusterizable[ID, BinaryScalarVector[Vb, Vs]](id, vectorizable)
{

	@transient lazy val vectorSeq = (vector.binary.toSeq, vector.scalar.toSeq)

	override def canEqual(a: Any): Boolean = a.isInstanceOf[ClusterizableM[ID, Obj, Vb, Vs]]

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: ClusterizableM[ID, Obj, Vb, Vs] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	override def hashCode(): Int =
	{
		val prime = 31
		var result = 1
		result = prime * result + vectorSeq._1.hashCode
		result = prime * result + vectorSeq._2.hashCode
		result = prime * result + id.hashCode
		result
	}

	def copy() = new ClusterizableM[ID, Obj, Vb, Vs](id, vectorizable)
}

/**
 * Clusterizable for Vector[Double] 
 **/
case class RealClusterizable[ID: Numeric, Obj, V <: Seq[Double]](
	override val id: ID,
	override val vectorizable: RealVectorizable[Obj, V],
	v2Tmp: V = Seq.empty[Double],
	clusterIDTmp: Int = Int.MaxValue
) extends ClusterizableExt[ID, Double, V](id, vectorizable, v2Tmp, clusterIDTmp)
{
	def setV2(newV2: V): this.type =
	{
		v2 = newV2
		this
	}

	def setClusterID(newCID: Int): this.type =
	{
		clusterID = newCID
		this
	}

	override def canEqual(a: Any): Boolean = a.isInstanceOf[RealClusterizable[ID, Obj, V]]

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: RealClusterizable[ID, Obj, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	def copy() = new RealClusterizable[ID, Obj, V](id, vectorizable, v2, clusterID)
}

/**
 * Clusterizable for Vector[Int] 
 **/
case class BinaryClusterizable[ID: Numeric, Obj, V <: Seq[Int]](
	override val id: ID,
	override val vectorizable: BinaryVectorizable[Obj, V],
	v2Tmp: V = Seq.empty[Int],
	clusterIDTmp: Int = Int.MaxValue
) extends ClusterizableExt[ID, Int, V](id, vectorizable, v2Tmp, clusterIDTmp)
{
	def setV2(newV2: V): this.type =
	{
		v2 = newV2
		this
	}

	def setClusterID(newCID: Int): this.type =
	{
		clusterID = newCID
		this
	}

	override def canEqual(a: Any): Boolean = a.isInstanceOf[BinaryClusterizable[ID, Obj, V]]

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: BinaryClusterizable[ID, Obj, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def copy() = new BinaryClusterizable[ID, Obj, V](id, vectorizable, v2, clusterID)
}