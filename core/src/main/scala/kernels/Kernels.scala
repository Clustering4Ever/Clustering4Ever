package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{exp, tanh, sqrt, Pi, log}
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import breeze.linalg.{DenseVector, DenseMatrix, sum, inv}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.{Distance, DistanceSeq, ContinuousDistance}
import clustering4ever.util.{SumVectors, ClusterBasicOperations, SimilarityMatrix}
import clustering4ever.scala.kernels.KernelNature._
/**
 *
 */
trait Kernel[V, +KA <: KernelArgs] {
	/**
	 *
	 */
	val kernelArgs: KA
	/**
	 *
	 */
	def obtainMode(v: V, env: GenSeq[V]) : V
}
/**
 *
 */
trait KernelSeq[N, V[N] <: Seq[N], +KA <: KernelArgs] extends Kernel[V[N], KA]
/**
 *
 */
object KernelUtils {
	/**
	 *
	 */
	def reducePreModeAndKernelValue[V[Double] <: Seq[Double]](gs: GenSeq[(V[Double], Double)]) = gs.reduce( (a, b) => (SumVectors.sumVectors(a._1, b._1), a._2 + b._2) )
	/**
	 *
	 */
	def computeModeAndCastIt[V[Double] <: Seq[Double]](preMode: V[Double], kernelValue: Double) = preMode.map(_ / kernelValue).asInstanceOf[V[Double]]
	/**
	 *
	 */
	def obtainPreMode[V[Double] <: Seq[Double]](vi: V[Double], kernelVal: Double) = vi.map(_ * kernelVal).asInstanceOf[V[Double]]
}

class KernelGaussian[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V]](val kernelArgs: KernelArgsGaussian[V, D]) extends KernelSeq[Double, V, KernelArgsGaussian[V, D]] {
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	def gaussianKernel(v1: V[Double], v2: V[Double]) = {
		val d = kernelArgs.metric.d(v1, v2)
		exp( - kernelArgs.bandwidth * d * d )
	}
	/**
	 *
	 */
	def obtainMode(v: V[Double], env: GenSeq[V[Double]]) : V[Double] = {
		val (preMode, kernelValue) = KernelUtils.reducePreModeAndKernelValue(
			env.map{ vi =>
				val kernelVal = gaussianKernel(v, vi)
		  		(KernelUtils.obtainPreMode(vi, kernelVal), kernelVal)
			}
		)
		KernelUtils.computeModeAndCastIt(preMode, kernelValue)
	}
}
/**
 *
 */
class KernelFlat[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V]](val kernelArgs: KernelArgsFlat[V, D]) extends KernelSeq[Double, V, KernelArgsFlat[V, D]] {
	/**
	 *
	 */
	def flatKernel(v1: V[Double], v2: V[Double]) = {
		val value = kernelArgs.metric.d(v1, v2) / (kernelArgs.bandwidth * kernelArgs.bandwidth)
		if( value <= kernelArgs.lambda ) 1D else 0D 
	}
	/**
	 *
	 */
	def obtainMode(v: V[Double], env: GenSeq[V[Double]]) : V[Double] = {
		val (preMode, kernelValue) = KernelUtils.reducePreModeAndKernelValue(
			env.map{ vi =>
				val kernelVal = flatKernel(v, vi)
		  		(KernelUtils.obtainPreMode(vi, kernelVal), kernelVal)
			}
		)
		KernelUtils.computeModeAndCastIt(preMode, kernelValue)
	}
}
/**
 *
 */
class KernelSigmoid[V[Double] <: Seq[Double]](val kernelArgs: KernelArgsSigmoid) extends Kernel[V[Double], KernelArgsSigmoid] {
	/**
	 *
	 */
	def sigmoidKernel(v1: V[Double], v2: V[Double]) = {
		val dotProd = SumVectors.dotProd(v1, v2)
		tanh(kernelArgs.a * dotProd + kernelArgs.b)
	}
	/**
	 *
	 */
	def obtainMode(v: V[Double], env: GenSeq[V[Double]]) : V[Double] = {
		val (preMode, kernelValue) = KernelUtils.reducePreModeAndKernelValue(
			env.map{ vi =>
				val kernelVal = sigmoidKernel(v, vi)
		  		(KernelUtils.obtainPreMode(vi, kernelVal), kernelVal)
			}
		)
		KernelUtils.computeModeAndCastIt(preMode, kernelValue)
	}
}

object GmmKernels {
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
}
/**
 * Investigate why specialization with Spire Numeric prevent compilations
 */
trait KnnKernel[
	N,
	V[N] <: Seq[N],
	D <: DistanceSeq[N, V],
	+Args <: KernelArgsKnnVec[N, V, D]
] extends KernelSeq[N, V, Args]  {
	/**
	 *
	 */
	val kernelArgs: Args
	/**
	 * @return knn
	 */
	def obtainKnn(v: V[N], env: Seq[V[N]]) = env.sortBy(kernelArgs.metric.d(v, _)).take(kernelArgs.k)
	/**
	 * The KNN kernel for real space, it select KNN using any real measure and compute the mode of them by looking for element which minimize average pair to pair distance
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	def obtainMode(v: V[N], env: GenSeq[V[N]]): V[N] = {
		val knn = obtainKnn(v, env.seq)
		val sm = SimilarityMatrix.simpleSimilarityMatrix(knn, kernelArgs.metric)
		sm.minBy{ case (_, distances) => distances.sum }._1
	}
}
/**
 *
 */
trait KnnKernelMetaReal[
	V[Double] <: Seq[Double],
	D <: ContinuousDistance[V],
	+Args <: KernelArgsKnnRealMeta[V, D]
] extends KnnKernel[Double, V, D, Args]
/**
 *
 */
class KnnKernelReal[
	V[Double] <: Seq[Double],
	D[V[Double] <: Seq[Double]] <: ContinuousDistance[V],
	Args[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V]] <: KernelArgsKnnReal[V, D]
](val kernelArgs: Args[V, D]) extends KnnKernelMetaReal[V, D[V], Args[V, D]]
/**
 *
 */
class KnnKernelEuclidean[
	V[Double] <: Seq[Double],
	D[V[Double] <: Seq[Double]] <: Euclidean[V],
	Args[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: Euclidean[V]] <: KernelArgsEuclideanKnn[V, D]
](val kernelArgs: Args[V, D]) extends KnnKernelMetaReal[V, D[V], Args[V, D]] {
	/**
	 * The KNN kernel for euclidean space, it select KNN using a Euclidean measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	override def obtainMode(v: V[Double], env: GenSeq[V[Double]]): V[Double] = {
		val knn = obtainKnn(v, env.seq)
		ClusterBasicOperations.obtainMean(knn)
	}
}