package clustering4ever.util

import _root_.scala.reflect.ClassTag
import _root_.scala.math.pow
import scala.collection.immutable

/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 */
object SumArrays
{
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumArraysNumerics[T](a: immutable.Seq[T], b: immutable.Seq[T])(implicit num: Numeric[T], ct: ClassTag[T]) : immutable.Seq[T] =
	{
		//for( i <- a.indices.toVector ) yield num.plus(a(i), b(i))
		a.zip(b).map{ case (c, d) => num.plus(c, d) }
	}
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	def sumColumnArrays[T](cluster: immutable.Seq[immutable.Seq[T]])(implicit num: Numeric[T], ct: ClassTag[T]): immutable.Seq[T] =
	{
		cluster.reduce(sumArraysNumerics(_, _))
	}
	/**
	 * Return the centroid of the given cluster
	 */
	def obtainMean(cluster: immutable.Seq[immutable.Seq[Double]]): immutable.Seq[Double] =
	{
		sumColumnArrays[Double](cluster).map( _ / cluster.size )
	}
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice(a: immutable.Seq[immutable.Seq[Double]], b: immutable.Seq[immutable.Seq[Double]]) =
	{
		//for( i <- a.indices.toVector ) yield sumArraysNumerics[Double](a(i), b(i))
		a.zip(b).map{ case (c, d) => sumArraysNumerics[Double](c, d) }
	}
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 */
	def diffDotProduct(v1: immutable.Seq[Double], v2: immutable.Seq[Double]) =
	{
		//( for( i <- v1.indices.toVector ) yield (v1(i) - v2(i)) ).map(pow(_, 2)).sum
		v1.zip(v2).map{ case (a, b) => pow(a - b, 2) }.sum		
	}
	/**
	 * Compute the mode of a cluster
	 */
	def obtainMode(cluster: immutable.Seq[immutable.Seq[Int]]): immutable.Seq[Int] =
	{
		sumColumnArrays[Int](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 )
	}

}