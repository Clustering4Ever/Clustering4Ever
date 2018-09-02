package clustering4ever.math.distances.binary

import clustering4ever.math.distances.BinaryDistance
import scala.collection.mutable
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.math.distances.BinaryClusterizableDistance

/**
 * @author Beck Gaël
 **/
trait HammingMeta extends Serializable {

	protected def hamming[V <: Seq[Int]](dot1: V, dot2: V): Double =
	{
		var d = 0D
		var i = 0
		while( i < dot1.size )
		{
			d += dot1(i) ^ dot2(i)
			i += 1
		}
		d
	}
}

class Hamming[V <: Seq[Int]] extends HammingMeta with BinaryDistance[V]
{
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  **/
	def d(dot1: V, dot2: V): Double = hamming[V](dot1, dot2)
}

class FastHamming extends Hamming[mutable.Buffer[Int]]
{
	override def d(dot1: mutable.Buffer[Int], dot2: mutable.Buffer[Int]): Double =
	{
		var sum = 0
		dot1.indices.foreach( i => sum += dot1(i) ^ dot2(i) )
		sum.toDouble
	}
}

class HammingClusterizable[ID: Numeric, Obj, V <: Seq[Int]] extends HammingMeta with BinaryClusterizableDistance[BinaryClusterizable[ID, Obj, V], V]
{
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  **/
	def d(dot1: BinaryClusterizable[ID, Obj, V], dot2: BinaryClusterizable[ID, Obj, V]): Double = hamming[V](dot1.vector, dot2.vector)

	def obtainClassicalDistance(): Hamming[V] = new Hamming[V]
}