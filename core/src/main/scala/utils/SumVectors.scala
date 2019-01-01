package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import scala.language.higherKinds
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.scala.vectors.{GVector, BinaryVector, ScalarVector, MixtVector}
import scala.collection.mutable
/**
 *
 */
object VectorsBasicOperationsImplicits {
	/**
	 *
	 */
	implicit def addRawScalarVectors[V <: Seq[Double]](v1: V, v2: V): V = {
		val builder = v1.genericBuilder.asInstanceOf[mutable.Builder[Double, V]]
		builder.sizeHint(v1.size)
		(0 until v1.size).foreach( i => builder += v1(i) + v2(i) )
		builder.result
	}
	/**
	 *
	 */
	implicit def addRawBinaryVectors[V <: Seq[Int]](v1: V, v2: V): V = {
		val builder = v1.genericBuilder.asInstanceOf[mutable.Builder[Int, V]]
		builder.sizeHint(v1.size)
		(0 until v1.size).foreach( i => builder += v1(i) + v2(i) )
		builder.result
	}
	/**
	 *
	 */
	implicit def addScalarVectors[V <: Seq[Double]](v1: ScalarVector[V], v2: ScalarVector[V]): ScalarVector[V] = {
		val builder = v1.vector.genericBuilder.asInstanceOf[mutable.Builder[Double, V]]
		builder.sizeHint(v1.vector.size)
		(0 until v1.vector.size).foreach( i => builder += v1.vector(i) + v2.vector(i) )
		new ScalarVector(builder.result)
	}
	/**
	 *
	 */
	implicit def addBinaryVectors[V <: Seq[Int]](v1: BinaryVector[V], v2: BinaryVector[V]): BinaryVector[V] = {
		val builder = v1.vector.genericBuilder.asInstanceOf[mutable.Builder[Int, V]]
		builder.sizeHint(v1.vector.size)
		(0 until v1.vector.size).foreach( i => builder += v1.vector(i) + v2.vector(i) )
		new BinaryVector(builder.result)
	}
	/**
	 *
	 */
	implicit def addMixtVectors[Vb <: Seq[Int], Vs <: Seq[Double]](v1: MixtVector[Vb, Vs], v2: MixtVector[Vb, Vs]): MixtVector[Vb, Vs] = {
		val binaryPart = addRawBinaryVectors(v1.binary, v2.binary)
		val scalarPart = addRawScalarVectors(v1.scalar, v2.scalar)
		new MixtVector(binaryPart, scalarPart)
	}
}
/**
 * Object which gather common operation on Vectors of any nature, aka scalar, binary, mixt
 */
object SumVectors {

	import VectorsBasicOperationsImplicits._
	/**
	 * add two vector no mather their types
	 */
	def sumVectors[V](v1: V, v2: V)(implicit f: (V, V) => V): V = f(v1, v2)
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	def sumColumnMatrix[V](cluster: GenSeq[V])(implicit f: (V, V) => V): V = cluster.reduce(sumVectors(_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[V <: Seq[Double], It[X] <: Seq[X]](a: It[V], b: It[V])(implicit f: (V, V) => V) = {	
		val range = (0 until a.size)
		val builder = a.genericBuilder.asInstanceOf[mutable.Builder[V, It[V]]]
		builder.sizeHint(a.size)
		range.foreach{ i => builder += sumVectors(a(i), b(i)) }
		builder.result
	}
	/**
	 * to tailrec
	 */
	def norm[V <: Seq[Double]](dot: V): Double = {
	  var s = 0D
	  var i = 0
	  while(i < dot.size) {
	    val v = dot(i)
	    s += v * v
	    i += 1
	  }
	  sqrt(s)
	}
	/**
	 * to tailrec
	 */
	def dotProd[V <: Seq[Double]](dot1: V, dot2: V): Double = {
		var dp = 0D
		var i = 0
		while(i < dot1.size) {
			dp += dot1(i) * dot2(i)
			i += 1
		}
		dp
	}
	/**
	 * to tailrec
	 */
	def dotProd[V <: Seq[Double]](dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = {
		var dp = 0D
		var i = 0
		while(i < dot1.vector.size) {
			dp += dot1.vector(i) * dot2.vector(i)
			i += 1
		}
		dp
	}
}