package clustering4ever.scala.kernels

import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.scala.math.{exp, tanh, pow}
import _root_.clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * Kernels gathers some of the most known kernels
 **/
object Kernels
{
	def flatKernel(v1: Array[Double], v2: Array[Double], bandwitch: Double, metric: ContinuousDistances) =
	{
		val λ = 1D
		if( metric.d(v1, v2) / pow(bandwitch, 2) <= λ ) 1D else 0D 
	}

	def gaussianKernel(v1: Array[Double], v2:Array[Double], bandwitch: Double, metric: ContinuousDistances) =
	{
		exp( - bandwitch * metric.d(v1, v2) )
	}

	def sigmoidKernel(v1: Array[Double], v2:Array[Double], a: Double = 1D, b: Double = 0D) =
	{
		var dotProd = 0D
		for( i <- v1.indices ) dotProd += v1(i) * v2(i)
		tanh(a * dotProd + b)
	}

	/**
	 * Compute the local mode of a point v knowing its environement env, the bandwitch, kernelType and metric
	 * @param kernelType can be either "gaussian" or "flat"
	 * @param bandwitch of the kernel approach
	 * @param metric is the dissimilarity measure used for kernels computation
	 **/
	def computeModesThroughKernels(v: Array[Double], env: Array[(Array[Double])], bandwitch: Double, kernelType: String, metric: ContinuousDistances) =
	{
		val kernel: (Array[Double], Array[Double], Double, ContinuousDistances) => Double = kernelType match
		{
			case gaussian if( gaussian == "gaussian" ) => gaussianKernel
			case flat if( flat == "flat" ) => flatKernel
			case _ => throw new Exception("you give a wrong kernel name")
		}

		val (preMod, kernelValue) = env.map{ vi =>
		{
		  val kernelVal = kernel(v, vi, bandwitch, metric)
		  (vi.map( _ * kernelVal ), kernelVal)
		}}.reduce( (a, b) => (SumArrays.sumArraysNumerics(a._1, b._1), a._2 + b._2) )

		val mod = preMod.map(_ / kernelValue)
		mod		
	}
}