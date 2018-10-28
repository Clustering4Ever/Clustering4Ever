package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.math.{exp, tanh, sqrt, Pi, log}
import scala.collection.GenSeq
import scala.language.higherKinds
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
	def flatKernel[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]](v1: V[Double], v2: V[Double], kernelArgs: KernelArgsFlat[V, D]) = {
		val value = kernelArgs.metric.d(v1, v2) / (kernelArgs.bandwidth * kernelArgs.bandwidth)
		if( value <= kernelArgs.lambda ) 1D else 0D 
	}
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	def gaussianKernel[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]](v1: V[Double], v2: V[Double], kernelArgs: KernelArgsGaussian[V, D]) = {
		val d = kernelArgs.metric.d(v1, v2)
		exp( - kernelArgs.bandwidth * d * d )
	}
	/**
	 *
	 */
	def sigmoidKernel[V <: Seq[Double]](v1: V, v2: V, kernelArgs: KernelArgsSigmoid) = {
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
	 * Compute the local mode of a point v knowing its environement env, the bandwidth and kernelArgs
	 * @param kernelArgs: an instanciation of KernelArgs class with neccessary arguments to run choiced kernel
	 * @param env: the environement where we have to looking for the mode
	 * @param v: the vector we are looking for its mode
	 */
	def obtainModeThroughKernel[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]](v: V[Double], env: GenSeq[V[Double]], kernelArgs: KernelArgsWithMetric[V[Double], D[V]]): V[Double] = {

		def reducePreModeAndKernelValue(gs: GenSeq[(V[Double], Double)]) = gs.reduce( (a, b) => (SumVectors.sumVectors(a._1, b._1), a._2 + b._2) )		
		
		val (preMode, kernelValue) = kernelArgs.kernelType match {
			case KernelNature.Gaussian => reducePreModeAndKernelValue(
				env.map{ vi =>
					val kernelVal = gaussianKernel(v, vi, kernelArgs.asInstanceOf[KernelArgsGaussian[V, D]])
			  		(vi.map(_ * kernelVal).asInstanceOf[V[Double]], kernelVal)
				}
			)
			case KernelNature.Flat => reducePreModeAndKernelValue(
				env.map{ vi =>
					val kernelVal = flatKernel(v, vi, kernelArgs.asInstanceOf[KernelArgsFlat[V, D]])
			  		(vi.map(_ * kernelVal).asInstanceOf[V[Double]], kernelVal)
				}
			)
			case KernelNature.Sigmoid => reducePreModeAndKernelValue(
				env.map{ vi =>
					val kernelVal = sigmoidKernel(v, vi, kernelArgs.asInstanceOf[KernelArgsSigmoid])
			  		(vi.map(_ * kernelVal).asInstanceOf[V[Double]], kernelVal)
				}
			)
		}
		preMode.map(_ / kernelValue).asInstanceOf[V[Double]]
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
	def knnKernel[O, D <: Distance[O]](v: O, env: Seq[O], kernelArgs: KernelArgsKNN[O, D]): O = {
		val knn = obtainKnn[O](v, env, kernelArgs.k, kernelArgs.metric)
		val sm = SimilarityMatrix.simpleSimilarityMatrix(knn, kernelArgs.metric)
		sm.minBy{ case (_, distances) => distances.sum }._1
	}
	/**
	 * The KNN kernel for euclidean space, it select KNN using a specific distance measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	def euclideanKnnKernel[V[Double] <: Seq[Double]](v: V[Double], env: Seq[V[Double]], kernelArgs: KernelArgsEuclideanKNN[V, Euclidean]): V[Double] = {
		val knn = obtainKnnVectors[Double, V[Double]](v, env, kernelArgs.k, kernelArgs.metric)
		ClusterBasicOperations.obtainMean(knn)
	}
	/**
	 *
	 */
	def vectorKnnKernel[@specialized(Int, Double) N: SNumeric, V <: Seq[N], D[V] <: Distance[V]](v: V, env: Seq[V], kernelArgs: KernelArgsKNN[V, D[V]]): V = knnKernel[V, D[V]](v, env, kernelArgs)
}