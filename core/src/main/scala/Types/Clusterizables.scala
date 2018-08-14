package clustering4ever.scala.clusterizables

import clustering4ever.scala.vectorizables.{Vectorizable, VectorizableG, VectorizableM, RealVectorizable, BinaryVectorizable}
import clustering4ever.util.LSH
import scala.collection.immutable

abstract class Clusterizable[ID: Numeric, Vector](val id: ID, val vectorizable: Vectorizable[Vector]) extends Serializable
{
	lazy val vector: Vector = vectorizable.toVector
}

/**
 * Generic clusterizable for both Vectors of Int or Double 
 **/
case class ClusterizableG[ID: Numeric, Obj, @specialized(Int, Double) T](override val id: ID, override val vectorizable: VectorizableG[T, Obj]) extends Clusterizable[ID, immutable.Vector[T]](id, vectorizable)
{
	@transient lazy val vectorSeq = vector.toSeq

	override def canEqual(a: Any): Boolean =
	{
		a.isInstanceOf[ClusterizableG[ID, Obj, T]]
	}

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: ClusterizableG[ID, Obj, T] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	override def hashCode(): Int =
	{
		val prime = 31
		var result = 1
		result = prime * result + vectorSeq.hashCode
		result = prime * result + id.hashCode
		result
	}

	def copy() =
	{
		new ClusterizableG[ID, Obj, T](id, vectorizable)
	}
}

/**
 * Generic clusterizable for both Mixt Vectors => (Vector[Int], Vector[Double]) 
 **/
case class ClusterizableM[ID: Numeric, Obj](override val id: ID, override val vectorizable: VectorizableM[Obj]) extends Clusterizable[ID, (immutable.Seq[Int], immutable.Seq[Double])](id, vectorizable)
{

	@transient lazy val vectorSeq = (vector._1.toSeq, vector._2.toSeq)

	override def canEqual(a: Any): Boolean =
	{
		a.isInstanceOf[ClusterizableM[ID, Obj]]
	}

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: ClusterizableM[ID, Obj] => that.canEqual(this) && that.hashCode == this.hashCode
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

	def copy() =
	{
		new ClusterizableM[ID, Obj](id, vectorizable)
	}
}

/**
 * Clusterizable for Vector[Double] 
 **/
case class RealClusterizable[ID: Numeric, Obj](override val id: ID, override val vectorizable: RealVectorizable[Obj]) extends Clusterizable[ID, immutable.Seq[Double]](id, vectorizable)
{
	@transient lazy val vectorSeq = vector.toSeq

	override def canEqual(a: Any): Boolean =
	{
		a.isInstanceOf[RealClusterizable[ID, Obj]]
	}

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: RealClusterizable[ID, Obj] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	override def hashCode(): Int =
	{
		val prime = 31
		var result = 1
		result = prime * result + vectorSeq.hashCode
		result = prime * result + id.hashCode
		result
	}

	def copy() =
	{
		new RealClusterizable[ID, Obj](id, vectorizable)
	}
}

/**
 * Clusterizable for Vector[Int] 
 **/
case class BinaryClusterizable[ID: Numeric, Obj](override val id: ID, override val vectorizable: BinaryVectorizable[Obj]) extends Clusterizable[ID, immutable.Seq[Int]](id, vectorizable)
{
	@transient lazy val vectorSeq = vector.toSeq

	override def canEqual(a: Any): Boolean =
	{
		a.isInstanceOf[BinaryClusterizable[ID, Obj]]
	}

	override def equals(that: Any): Boolean =
	{
		that match
		{
		  case that: BinaryClusterizable[ID, Obj] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
  
	override def hashCode(): Int =
	{
		val prime = 31
		var result = 1
		result = prime * result + vectorSeq.hashCode
		result = prime * result + id.hashCode
		result
	}

	def copy() =
	{
		new BinaryClusterizable[ID, Obj](id, vectorizable)
	}
}