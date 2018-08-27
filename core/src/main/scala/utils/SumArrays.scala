package clustering4ever.util

import scala.reflect.ClassTag
import scala.math.pow
import scala.collection.{immutable, GenSeq}

/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 */
object SumArrays
{
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumArraysNumerics[T: ClassTag](a: immutable.Seq[T], b: immutable.Seq[T])(implicit num: Numeric[T]): immutable.Seq[T] =
		a.zip(b).map{ case (c, d) => num.plus(c, d) }
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumArraysNumericsGen[T: ClassTag, V <: immutable.Seq[T]](a: V, b: V)(implicit num: Numeric[T]): V =
		a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[V]
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	def sumColumnArrays[T: ClassTag](cluster: GenSeq[immutable.Seq[T]])(implicit num: Numeric[T]): immutable.Seq[T] =
		cluster.reduce(sumArraysNumerics[T](_, _))
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	def sumColumnArraysGen[T: ClassTag, V <: immutable.Seq[T]](cluster: GenSeq[V])(implicit num: Numeric[T]): V =
		cluster.reduce(sumArraysNumericsGen[T, V](_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice(a: immutable.Seq[immutable.Seq[Double]], b: immutable.Seq[immutable.Seq[Double]]) =
		a.zip(b).map{ case (c, d) => sumArraysNumerics[Double](c, d) }
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 */
	def diffDotProduct(v1: immutable.Seq[Double], v2: immutable.Seq[Double]) =
		v1.zip(v2).map{ case (a, b) => pow(a - b, 2) }.sum		
	/**
	 * Return the centroid of the given cluster
	 */
	def obtainMean(cluster: GenSeq[immutable.Seq[Double]]): immutable.Seq[Double] =
		sumColumnArrays[Double](cluster).map( _ / cluster.size )
	/**
	 * Return the centroid of the given cluster
	 */
	def obtainMeanGen[V <: immutable.Seq[Double]](cluster: GenSeq[V]): V =
		sumColumnArraysGen[Double, V](cluster).map( _ / cluster.size ).asInstanceOf[V]
	/**
	 * Compute the mode of a cluster
	 */
	def obtainMode(cluster: GenSeq[immutable.Seq[Int]]): immutable.Seq[Int] =
		sumColumnArrays[Int](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 )
	/**
	 * Return the centroid of the given cluster
	 */
	def obtainModeGen[V <: immutable.Seq[Int]](cluster: GenSeq[V]): V =
		sumColumnArraysGen[Int, V](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 ).asInstanceOf[V]
}