package clustering4ever.scala.kernels

import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.scala.math.{exp, tanh, pow}
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.scala.kernels.KernelNature._
/**
 * @author Beck Gaël
 * Kernels gathers some of the most known kernels
 **/
object Kernels
{
	def flatKernel(v1: Vector[Double], v2: Vector[Double], bandwidth: Double, metric: ContinuousDistances) =
	{
		val λ = 1D
		if( metric.d(v1, v2) / pow(bandwidth, 2) <= λ ) 1D else 0D 
	}

	def flatKernel(v1: Vector[Double], v2: Vector[Double], bandwidth: Double, metric: ContinuousDistances, λ: Double = 1D) =
	{
		if( metric.d(v1, v2) / pow(bandwidth, 2) <= λ ) 1D else 0D 
	}
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-λ|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - λ is the bandwitch
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 **/
	def gaussianKernel(v1: Vector[Double], v2: Vector[Double], bandwidth: Double, metric: ContinuousDistances) =
	{
		exp( - bandwidth * pow(metric.d(v1, v2), 2) )
	}

	def sigmoidKernel(v1: Vector[Double], v2: Vector[Double], a: Double = 1D, b: Double = 0D) =
	{
		var dotProd = 0D
		for( i <- v1.indices ) dotProd += v1(i) * v2(i)
		tanh(a * dotProd + b)
	}

	/**
	 * Compute the local mode of a point v knowing its environement env, the bandwidth, kernelType and metric
	 * @param kernelType can be either "gaussian" or "flat", if "flat" λ = 1
	 * @param bandwidth of the kernel approach
	 * @param metric is the dissimilarity measure used for kernels computation
	 **/
	def obtainModeThroughKernel(v: Vector[Double], env: Seq[Vector[Double]], bandwidth: Double, kernelType: KernelType, metric: ContinuousDistances) =
	{
		val kernel: (Vector[Double], Vector[Double], Double, ContinuousDistances) => Double = kernelType match
		{
			case KernelNature.Gaussian => gaussianKernel
			case KernelNature.Flat => flatKernel
		}

		val (preMod, kernelValue) = env.map{ vi =>
		{
		  val kernelVal = kernel(v, vi, bandwidth, metric)
		  (vi.map( _ * kernelVal ), kernelVal)
		}}.reduce( (a, b) => (SumArrays.sumArraysNumerics(a._1, b._1), a._2 + b._2) )

		val mode = preMod.map(_ / kernelValue)
		mode		
	}

	def obtainModeThroughSigmoid(v: Vector[Double], env: Seq[Vector[Double]], a: Double, b: Double) =
	{
		val (preMod, kernelValue) = env.map{ vi =>
		{
		  val kernelVal = sigmoidKernel(v, vi, a, b)
		  (vi.map( _ * kernelVal ), kernelVal)
		}}.reduce( (a, b) => (SumArrays.sumArraysNumerics(a._1, b._1), a._2 + b._2) )

		val mode = preMod.map(_ / kernelValue)
		mode		
	}

	/**
	 * The KNN kernel, it select KNN using a specific distance measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 **/
	def knnKernel(v: Vector[Double], env: Seq[Vector[Double]], k: Int, metric: ContinuousDistances) =
	{
		val knn = env.map( v2 => (v2, metric.d(v, v2)) ).sortBy(_._2).take(k).map(_._1)
		SumArrays.obtainMean(knn)
	}
}