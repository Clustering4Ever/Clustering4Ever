package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import scala.language.higherKinds
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.vectors.{GVector, BinaryVector, ScalarVector, MixedVector}
import scala.collection.mutable
/**
 *
 */
object FromArrayToSeq extends Serializable {
	/**
	 *
	 */
	final def arrayToSimpleSeq[N: SNumeric, V <: Seq[N]](a: Array[N]) = {
		val builder = a.genericBuilder[N].asInstanceOf[mutable.Builder[N, V]]
		builder.sizeHint(a.size)
		builder ++= a
		builder.result
	}
	/**
	 *
	 */
	final def arrayToScalarSeq[V <: Seq[Double]](a: Array[Double]) = arrayToSimpleSeq[Double, V](a)
	/**
	 *
	 */
	final def arrayToBinarySeq[V <: Seq[Int]](a: Array[Int]) = arrayToSimpleSeq[Int, V](a)

}
/**
 *
 */
object VectorsAddOperationsImplicits extends Serializable {
	/**
	 * Check perf before use it
	 */
	final private def addRawSimpleVector[N, V <: Seq[N]](v1: V, v2: V)(implicit num: SNumeric[N]): V = {
		val builder = v1.genericBuilder[N].asInstanceOf[mutable.Builder[N, V]]
		builder.sizeHint(v1.size)
		(0 until v1.size).foreach( i => builder += num.plus(v1(i), v2(i)) )
		builder.result
	}
	/**
	 *
	 */
	final implicit def addRawScalarVectors[V <: Seq[Double]](v1: V, v2: V): V = {
		addRawSimpleVector[Double, V](v1, v2)
	}
	/**
	 *
	 */
	final implicit def addRawBinaryVectors[V <: Seq[Int]](v1: V, v2: V): V = {
		addRawSimpleVector[Int, V](v1, v2)
	}
	/**
	 *
	 */
	final implicit def addScalarVectors[V <: Seq[Double]](v1: ScalarVector[V], v2: ScalarVector[V]): ScalarVector[V] = {
		ScalarVector(addRawScalarVectors(v1.vector, v2.vector))
	}
	/**
	 *
	 */
	final implicit def addBinaryVectors[V <: Seq[Int]](v1: BinaryVector[V], v2: BinaryVector[V]): BinaryVector[V] = {
		BinaryVector(addRawBinaryVectors(v1.vector, v2.vector))
	}
	/**
	 *
	 */
	final implicit def addMixedVectors[Vb <: Seq[Int], Vs <: Seq[Double]](v1: MixedVector[Vb, Vs], v2: MixedVector[Vb, Vs]): MixedVector[Vb, Vs] = {
		val binaryPart = addRawBinaryVectors(v1.binary, v2.binary)
		val scalarPart = addRawScalarVectors(v1.scalar, v2.scalar)
		MixedVector(binaryPart, scalarPart)
	}
}
/**
 * Object which gather common operation on Vectors of any nature, aka scalar, binary, mixt
 */
object SumVectors extends Serializable {

	import VectorsAddOperationsImplicits._
	/**
	 * add two vector no mather their types
	 */
	final def sumVectors[V](v1: V, v2: V)(implicit f: (V, V) => V): V = f(v1, v2)
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	final def sumColumnMatrix[V](cluster: GenSeq[V])(implicit f: (V, V) => V): V = cluster.reduce(sumVectors(_, _))
	/**
	 * Reduce Seq of multiple vectors
	 */
	final def sumAlignedVectorsMatrice[V <: Seq[Double], S[X] <: Seq[X]](a: S[V], b: S[V])(implicit f: (V, V) => V) = {
		val range = (0 until a.size)
		val builder = a.genericBuilder[V].asInstanceOf[mutable.Builder[V, S[V]]]
		builder.sizeHint(a.size)
		range.foreach{ i => builder += sumVectors(a(i), b(i)) }
		builder.result
	}
	/**
	 *
	 */
	final def dotProduct[V <: Seq[Double]](dot1: V, dot2: V): Double = {
		@annotation.tailrec
		def go(i: Int, sum: Double): Double = {
			val res = sum + dot1(i) * dot2(i)
			if(i < dot1.size - 1) go(i + 1, res)
			else res
		}
		go(0, 0D)
	}
	/**
	 *
	 */
	final def dotProduct[V <: Seq[Double]](dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = dotProduct(dot1.vector, dot2.vector)
	/**
	 *
	 */
	final def euclideanNorm[V <: Seq[Double]](dot: V): Double = sqrt(dotProduct(dot, dot))
	/**
	 *
	 */
	final def euclideanNorm[V <: Seq[Double]](dot: ScalarVector[V]): Double = euclideanNorm(dot.vector)
}