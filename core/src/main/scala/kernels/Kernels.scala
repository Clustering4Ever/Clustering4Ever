package clustering4ever.scala.kernels

import scala.collection.{immutable, GenSeq}
import scala.math.{exp, tanh, pow}
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.Distance
import clustering4ever.util.SumArrays
import clustering4ever.scala.kernels.KernelNature._
import clustering4ever.util.SimilarityMatrix

/**
 * @author Beck Gaël
 * Kernels gathers some of the most known kernels
 **/
object Kernels
{
	def flatKernel(v1: Seq[Double], v2: Seq[Double], bandwidth: Double, metric: ContinuousDistances) =
	{
		val λ = 1D
		if( metric.d(v1, v2) / pow(bandwidth, 2) <= λ ) 1D else 0D 
	}

	def flatKernel(v1: Seq[Double], v2: Seq[Double], bandwidth: Double, metric: ContinuousDistances, λ: Double = 1D) = if( metric.d(v1, v2) / pow(bandwidth, 2) <= λ ) 1D else 0D 

	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-λ|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - λ is the bandwitch
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 **/
	def gaussianKernel(v1: Seq[Double], v2: Seq[Double], bandwidth: Double, metric: ContinuousDistances) = exp( - bandwidth * pow(metric.d(v1, v2), 2) )

	def sigmoidKernel(v1: Seq[Double], v2: Seq[Double], a: Double = 1D, b: Double = 0D) =
	{
		val dotProd = v1.zip(v2).map{ case (a, b) => a * b }.sum
		tanh(a * dotProd + b)
	}

	/**
	 * Compute the local mode of a point v knowing its environement env, the bandwidth, kernelType and metric
	 * @param kernelType can be either "gaussian" or "flat", if "flat" λ = 1
	 * @param bandwidth of the kernel approach
	 * @param metric is the dissimilarity measure used for kernels computation
	 **/
	def obtainModeThroughKernel(v: Seq[Double], env: GenSeq[Seq[Double]], bandwidth: Double, kernelType: KernelType, metric: ContinuousDistances) =
	{
		val kernel: (Seq[Double], Seq[Double], Double, ContinuousDistances) => Double = kernelType match
		{
			case KernelNature.Gaussian => gaussianKernel
			case KernelNature.Flat => flatKernel
		}

		val (preMode, kernelValue) = env.map{ vi =>
		{
		  val kernelVal = kernel(v, vi, bandwidth, metric)
		  (vi.map( _ * kernelVal ), kernelVal)
		}}.reduce( (a, b) => (SumArrays.sumArraysNumerics[Double](a._1, b._1), a._2 + b._2) )

		val mode = preMode.map(_ / kernelValue)
		mode		
	}

	def obtainModeThroughSigmoid(v: Seq[Double], env: GenSeq[Seq[Double]], a: Double, b: Double) =
	{
		val (preMode, kernelValue) = env.map{ vi =>
		{
		  val kernelVal = sigmoidKernel(v, vi, a, b)
		  (vi.map( _ * kernelVal ), kernelVal)
		}}.reduce( (a, b) => (SumArrays.sumArraysNumerics[Double](a._1, b._1), a._2 + b._2) )

		val mode = preMode.map(_ / kernelValue)
		mode		
	}

	/**
	 * The KNN kernel for euclidean space, it select KNN using a specific distance measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 **/
	def euclideanKnnKernel(v: Seq[Double], env: Seq[Seq[Double]], k: Int, metric: Euclidean) =
	{
		val knn = env.sortBy( v2 => metric.d(v, v2) ).take(k)
		SumArrays.obtainMean(knn)
	}

	def knnKernel[Obj](v: Obj, env: Seq[Obj], k: Int, metric: Distance[Obj]) =
	{
		val knn = env.sortBy( v2 => metric.d(v, v2) ).take(k)
		val sm = SimilarityMatrix.simpleSimilarityMatrix(knn, metric)
		sm.map{ case (v, dists) => (v, dists.sum) }.minBy(_._2)._1
	}
}