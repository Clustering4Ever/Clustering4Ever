package clustering4ever.scala.kernels

import scala.collection.{immutable, GenSeq}
import scala.math.{exp, tanh, pow}
import clustering4ever.math.distances.ContinuousDistance
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
	def flatKernel[V <: Seq[Double]](v1: V, v2: V, bandwidth: Double, metric: ContinuousDistance[V]) = if( metric.d(v1, v2) / pow(bandwidth, 2) <= 1D ) 1D else 0D 

	def flatKernel[V <: Seq[Double]](v1: V, v2: V, bandwidth: Double, metric: ContinuousDistance[V], λ: Double = 1D) = if( metric.d(v1, v2) / pow(bandwidth, 2) <= λ ) 1D else 0D 
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-λ|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - λ is the bandwitch
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 **/
	def gaussianKernel[V <: Seq[Double]](v1: V, v2: V, bandwidth: Double, metric: ContinuousDistance[V]) = exp( - bandwidth * pow(metric.d(v1, v2), 2) )

	def sigmoidKernel[V <: Seq[Double]](v1: V, v2: V, a: Double = 1D, b: Double = 0D) =
	{
		var dotProd = 0D
		v1.zip(v2).foreach{ case (a, b) => dotProd += a * b }
		tanh(a * dotProd + b)
	}

	private def reducePreModeAndKernelValue[V <: Seq[Double]](gs: GenSeq[(V, Double)]) = gs.reduce( (a, b) => (SumArrays.sumArraysNumericsGen[Double, V](a._1, b._1), a._2 + b._2) )		
	/**
	 * Compute the local mode of a point v knowing its environement env, the bandwidth, kernelType and metric
	 * @param kernelType can be either "gaussian" or "flat", if "flat" λ = 1
	 * @param bandwidth of the kernel approach
	 * @param metric is the dissimilarity measure used for kernels computation
	 **/

	private def computeModeAndCastIt[V <: Seq[Double]](preMode: V, kernelValue: Double) = preMode.map(_ / kernelValue).asInstanceOf[V]

	def obtainModeThroughKernel[V <: Seq[Double]](v: V, env: GenSeq[V], bandwidth: Double, kernelType: KernelType, metric: ContinuousDistance[V]): V =
	{
		val kernel: (V, V, Double, ContinuousDistance[V]) => Double = kernelType match
		{
			case KernelNature.Gaussian => gaussianKernel[V]
			case KernelNature.Flat => flatKernel[V]
		}

		val (preMode, kernelValue) = reducePreModeAndKernelValue[V](
			env.map{ vi =>
			{
			  val kernelVal = kernel(v, vi, bandwidth, metric)
			  (vi.map(_ * kernelVal).asInstanceOf[V], kernelVal)
			}}
		)
		computeModeAndCastIt[V](preMode, kernelValue)
	}

	def obtainModeThroughSigmoid[V <: Seq[Double]](v: V, env: GenSeq[V], a: Double, b: Double): V =
	{
		val (preMode, kernelValue) = reducePreModeAndKernelValue[V](
			env.map{ vi =>
			{
			  val kernelVal = sigmoidKernel(v, vi, a, b)
			  (vi.map(_ * kernelVal).asInstanceOf[V], kernelVal)
			}}
		)
		computeModeAndCastIt[V](preMode, kernelValue)
	}

	private def obtainKnn[Obj](v: Obj, env: Seq[Obj], k: Int, metric: Distance[Obj]) = env.sortBy( v2 => metric.d(v, v2) ).take(k)

	/**
	 * The KNN kernel for euclidean space, it select KNN using a specific distance measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 **/
	def euclideanKnnKernel[V <: Seq[Double]](v: V, env: Seq[V], k: Int, metric: Euclidean[V]): V =
	{
		val knn = obtainKnn[V](v, env, k, metric)
		SumArrays.obtainMeanGen[V](knn)
	}

	def knnKernel[Obj](v: Obj, env: Seq[Obj], k: Int, metric: Distance[Obj]): Obj =
	{
		val knn = obtainKnn[Obj](v, env, k, metric)
		val sm = SimilarityMatrix.simpleSimilarityMatrix(knn, metric)
		sm.minBy{ case (_, dists) => dists.sum }._1
	}
}