package clustering4ever.stats

import _root_.scala.math.{sqrt, pow}
import _root_.clustering4ever.util.SumArrays

object Stats
{

	def mean(vectors: Array[Array[Double]]) =
	{
		vectors.reduce( (a, b) => for( i <- a.indices.toArray ) yield (a(i) + b(i)) ).map(_ / vectors.size)
	}

	def sd(vectors: Array[Array[Double]], mean: Array[Double]) =
	{
		sqrt(
			vectors.map( v =>
			{
				var sum = 0D
				for( i <- v.indices ) sum += pow(v(i) - mean(i), 2)
				sum
			}).sum
		)
	}

	def reduceMatriceColumns(a: Array[Double], b: Array[Double]) =
	{
		for( i <- a.indices.toArray ) yield (a(i) + b(i))
	}

	def reduceMultipleMatriceColumns(a: Array[Array[Double]], b: Array[Array[Double]]) =
	{
		for( i <- a.indices.toArray ) yield SumArrays.sumArraysNumerics(a(i), b(i))
	}

	def diffDotProduct(v1: Array[Double], v2: Array[Double]) =
	{
		( for( i <- v1.indices.toArray ) yield (v1(i) - v2(i)) ).map(pow(_, 2)).sum			
	}
}