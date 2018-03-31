package clustering4ever.scala.kernels

import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.scala.math.{exp, tanh, pow}
import _root_.clustering4ever.util.SumArrays

object Kernels
{
	def flatKernel(v1: Array[Double], v2: Array[Double], bandwitch: Double, metric: ContinuousDistances) =
	{
		val λ = 1D
		if( metric.distance(v1, v2) / pow(bandwitch, 2) <= λ ) 1D else 0D 
	}

	def gaussianKernel(v1: Array[Double], v2:Array[Double], bandwitch: Double, metric: ContinuousDistances) =
	{
		exp( - bandwitch * metric.distance(v1, v2) )
	}

	def sigmoidKernel(v1: Array[Double], v2:Array[Double], a: Double = 1D, b: Double = 0D) =
	{
		var dotProd = 0D
		for( i <- v1.indices ) dotProd += v1(i) * v2(i)
		tanh(a * dotProd + b)
	}
}