package clustering4ever.scala.kernels

import scala.math.{exp, tanh}
import breeze.linalg._
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.Distance
import clustering4ever.util.SumVectors
import clustering4ever.scala.kernels.KernelNature._
import clustering4ever.util.SimilarityMatrix

/**
 * @author Beck Gaël
 * Kernels gathers some of the most known kernels
 **/
object Kernels {

	def flatKernel[S <: Seq[Double]](v1: S, v2: S, bandwidth: Double, metric: ContinuousDistance[S]) = if( metric.d(v1, v2) / (bandwidth * bandwidth) <= 1D ) 1D else 0D 

	def flatKernel[S <: Seq[Double]](v1: S, v2: S, bandwidth: Double, metric: ContinuousDistance[S], λ: Double = 1D) = if( metric.d(v1, v2) / (bandwidth * bandwidth) <= λ ) 1D else 0D 
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-λ|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - λ is the bandwitch
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	def gaussianKernel[S <: Seq[Double]](v1: S, v2: S, bandwidth: Double, metric: ContinuousDistance[S]) = {
		val d = metric.d(v1, v2)
		exp( - bandwidth * d * d )
	}

	def gmm(v1: DenseVector[Double], mean: DenseVector[Double], invCovMat: DenseMatrix[Double]) = {
		val diff = v1 - mean
		exp(sum(- 0.5 * diff.t * invCovMat * diff))
	}

	def sigmoidKernel[S <: Seq[Double]](v1: S, v2: S, a: Double = 1D, b: Double = 0D) = {
		val dotProd = SumVectors.dotProd(v1, v2)
		tanh(a * dotProd + b)
	}

	private[this] def reducePreModeAndKernelValue[S <: Seq[Double]](gs: Seq[(S, Double)]) = gs.reduce( (a, b) => (SumVectors.sumVectors[Double, S](a._1, b._1), a._2 + b._2) )		

	private[this] def computeModeAndCastIt[S <: Seq[Double]](preMode: S, kernelValue: Double) = preMode.map(_ / kernelValue).asInstanceOf[S]

	/**
	 * Compute the local mode of a point v knowing its environement env, the bandwidth, kernelType and metric
	 * @param kernelType can be either "gaussian" or "flat", if "flat" λ = 1
	 * @param bandwidth of the kernel approach
	 * @param metric is the dissimilarity measure used for kernels computation
	 */
	def obtainModeThroughKernel[S <: Seq[Double]](v: S, env: Seq[S], bandwidth: Double, kernelType: KernelType, metric: ContinuousDistance[S]): S = {
		val kernel: (S, S, Double, ContinuousDistance[S]) => Double = kernelType match {
			case KernelNature.Gaussian => gaussianKernel[S]
			case KernelNature.Flat => flatKernel[S]
		}

		val (preMode, kernelValue) = reducePreModeAndKernelValue[S](
			env.map{ vi =>
			  val kernelVal = kernel(v, vi, bandwidth, metric)
			  (vi.map(_ * kernelVal).asInstanceOf[S], kernelVal)
			}
		)
		computeModeAndCastIt[S](preMode, kernelValue)
	}

	def obtainModeThroughSigmoid[S <: Seq[Double]](v: S, env: Seq[S], a: Double, b: Double): S = {
		val (preMode, kernelValue) = reducePreModeAndKernelValue[S](
			env.map{ vi =>
			  val kernelVal = sigmoidKernel(v, vi, a, b)
			  (vi.map(_ * kernelVal).asInstanceOf[S], kernelVal)
			}
		)
		computeModeAndCastIt[S](preMode, kernelValue)
	}

	private[this] def obtainKnn[Obj](v: Obj, env: Seq[Obj], k: Int, metric: Distance[Obj]) = env.sortBy( v2 => metric.d(v, v2) ).take(k)
	/**
	 * The KNN kernel for euclidean space, it select KNN using a specific distance measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	def euclideanKnnKernel[S <: Seq[Double]](v: S, env: Seq[S], k: Int, metric: Euclidean[S]): S = {
		val knn = obtainKnn[S](v, env, k, metric)
		SumVectors.obtainMean[S](knn)
	}

	def knnKernel[Obj](v: Obj, env: Seq[Obj], k: Int, metric: Distance[Obj]): Obj = {
		val knn = obtainKnn[Obj](v, env, k, metric)
		val sm = SimilarityMatrix.simpleSimilarityMatrix(knn, metric)
		sm.minBy{ case (_, dists) => dists.sum }._1
	}
}