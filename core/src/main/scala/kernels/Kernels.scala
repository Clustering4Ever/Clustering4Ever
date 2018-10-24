package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.math.{exp, tanh, sqrt, Pi, log}
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import breeze.linalg.{DenseVector, DenseMatrix, sum, inv}
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.Distance
import clustering4ever.util.{SumVectors, ClusterBasicOperations, SimilarityMatrix}
import clustering4ever.scala.kernels.KernelNature._
/**
 * Kernels gathers some of the most known kernels
 */
object Kernels {
	/**
	 *
	 */
	def flatKernel[V <: Seq[Double], D <: ContinuousDistance[V]](v1: V, v2: V, kernelArgs: KernelArgs[V, D]) = {
		val value = kernelArgs.metric.get.d(v1, v2) / (kernelArgs.bandwidth.get * kernelArgs.bandwidth.get)
		if( value <= kernelArgs.lambda ) 1D else 0D 
	}
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	def gaussianKernel[V <: Seq[Double], D <: ContinuousDistance[V]](v1: V, v2: V, kernelArgs: KernelArgs[V, D]) = {
		val d = kernelArgs.metric.get.d(v1, v2)
		exp( - kernelArgs.bandwidth.get * d * d )
	}
	/**
	 *
	 */
	def sigmoidKernel[V <: Seq[Double]](v1: V, v2: V, kernelArgs: KernelArgs[V, _]) = {
		val dotProd = SumVectors.dotProd(v1, v2)
		tanh(kernelArgs.a * dotProd + kernelArgs.b)
	}
	/**
	 *
	 */
	def multivariateGaussianKernelLog(v1: DenseVector[Double], mean: DenseVector[Double], invSigma: DenseMatrix[Double]): Double = {
		val diff = v1 - mean
		- diff.t * invSigma * diff * 0.5
	}
	/**
	 *
	 */
	def multivariateGaussianKernel(v1: DenseVector[Double], mean: DenseVector[Double], invSigma: DenseMatrix[Double]): Double = {
		exp(multivariateGaussianKernelLog(v1, mean, invSigma))
	}
	/**
	 * Compute the local mode of a point v knowing its environement env, the bandwidth, kernelType and metric
	 * @param kernelType can be either "gaussian" or "flat", if "flat" lambda = 1
	 * @param bandwidth of the kernel approach
	 * @param metric is the dissimilarity measure used for kernels computation
	 */
	def obtainModeThroughKernel[V <: Seq[Double], D <: ContinuousDistance[V]](v: V, env: GenSeq[V], kernelArgs: KernelArgs[V, D]): V = {

		def reducePreModeAndKernelValue(gs: GenSeq[(V, Double)]) = gs.reduce( (a, b) => (SumVectors.sumVectors[Double, V](a._1, b._1), a._2 + b._2) )		
		
		def computeModeAndCastIt(preMode: V, kernelValue: Double) = preMode.map(_ / kernelValue).asInstanceOf[V]
		
		val kernel: (V, V, KernelArgs[V, D]) => Double = kernelArgs.kernelType match {
			case KernelNature.Gaussian => gaussianKernel[V, D]
			case KernelNature.Flat => flatKernel[V, D]
			case KernelNature.Sigmoid => sigmoidKernel[V]
		}
		val (preMode, kernelValue) = reducePreModeAndKernelValue(
			env.map{ vi =>
			  val kernelVal = kernel(v, vi, kernelArgs)
			  (vi.map(_ * kernelVal).asInstanceOf[V], kernelVal)
			}
		)
		computeModeAndCastIt(preMode, kernelValue)
	}
	/**
	 *
	 */
	private[this] def obtainKnn[O](v: O, env: Seq[O], k: Int, metric: Distance[O]) = env.sortBy( v2 => metric.d(v, v2) ).take(k)
	/**
	 *
	 */
	private[this] def obtainKnnVectors[@specialized(Int, Double) N: SNumeric, V <: Seq[N]](v: V, env: Seq[V], k: Int, metric: Distance[V]) = obtainKnn[V](v, env, k, metric)
	/**
	 *
	 */
	def knnKernel[O, D <: Distance[O]](v: O, env: Seq[O], kernelArgs: KernelArgs[O, D]): O = {
		val knn = obtainKnn[O](v, env, kernelArgs.k.get, kernelArgs.metric.get)
		val sm = SimilarityMatrix.simpleSimilarityMatrix(knn, kernelArgs.metric.get)
		sm.minBy{ case (_, distances) => distances.sum }._1
	}
	/**
	 * The KNN kernel for euclidean space, it select KNN using a specific distance measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	def euclideanKnnKernel[V <: Seq[Double]](v: V, env: Seq[V], kernelArgs: KernelArgs[V, Euclidean[V]]): V = {
		val knn = obtainKnnVectors[Double, V](v, env, kernelArgs.k.get, kernelArgs.metric.get)
		ClusterBasicOperations.obtainMean[V](knn)
	}
	/**
	 *
	 */
	def vectorKnnKernel[@specialized(Int, Double) N: SNumeric, V <: Seq[N], D <: Distance[V]](v: V, env: Seq[V], kernelArgs: KernelArgs[V, D]): V = knnKernel[V, D](v, env, kernelArgs)
}