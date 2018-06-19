package clustering4ever.util

import _root_.scala.reflect.ClassTag
import _root_.scala.math.pow

/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 **/
object SumArrays
{
	/**
	 * Sum two vector of Numeric into one
	 **/
	def sumArraysNumerics[T](a: Vector[T], b: Vector[T])(implicit num: Numeric[T], ct: ClassTag[T]) : Vector[T] =
	{
		for( i <- a.indices.toVector ) yield( num.plus(a(i), b(i)) )
	}
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 **/
	def sumColumnArrays[T](cluster: Seq[Vector[T]])(implicit num: Numeric[T], ct: ClassTag[T]): Vector[T] =
	{
		cluster.reduce(sumArraysNumerics(_, _))
	}
	/**
	 * Return the centroid of the given cluster
	 **/
	def obtainMean(cluster: Seq[Vector[Double]]): Vector[Double] =
	{
		sumColumnArrays(cluster).map( _ / cluster.size )
	}
	/**
	 * Reduce Array of multiple vectors
	 **/
	def reduceMultipleVectorsMatrice(a: Vector[Vector[Double]], b: Vector[Vector[Double]]) =
	{
		for( i <- a.indices.toVector ) yield sumArraysNumerics(a(i), b(i))
	}
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 **/
	def diffDotProduct(v1: Vector[Double], v2: Vector[Double]) =
	{
		( for( i <- v1.indices.toVector ) yield (v1(i) - v2(i)) ).map(pow(_, 2)).sum			
	}
}