package org.clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{exp, tanh, sqrt, Pi, log}
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import breeze.linalg.{DenseVector, DenseMatrix, sum, inv}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.{Distance, ContinuousDistance}
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations, SimilarityMatrix}
import org.clustering4ever.scala.kernels.KernelNature._
/**
 *
 */
trait Kernel[V, +KA <: KernelArgs] extends Serializable {
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
object KernelUtils {
	import org.clustering4ever.util.VectorsBasicOperationsImplicits._
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
/**
 *
 */
case class KernelGaussian[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val kernelArgs: KernelArgsGaussian[V, D]) extends Kernel[V[Double], KernelArgsGaussian[V, D]] {
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	def gaussianKernel(v1: V[Double], altVectors: V[Double]) = {
		val d = kernelArgs.metric.d(v1, altVectors)
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
case class KernelFlat[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val kernelArgs: KernelArgsFlat[V, D]) extends Kernel[V[Double], KernelArgsFlat[V, D]] {
	/**
	 *
	 */
	def flatKernel(v1: V[Double], altVectors: V[Double]) = {
		val value = kernelArgs.metric.d(v1, altVectors) / (kernelArgs.bandwidth * kernelArgs.bandwidth)
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
case class KernelSigmoid[V[Double] <: Seq[Double]](val kernelArgs: KernelArgsSigmoid) extends Kernel[V[Double], KernelArgsSigmoid] {
	/**
	 *
	 */
	def sigmoidKernel(v1: V[Double], altVectors: V[Double]) = {
		val dotProd = SumVectors.dotProd(v1, altVectors)
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
/**
 * working on GMM
 **/
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
	V,
	D <: Distance[V],
	+Args <: KernelArgsKnn[V, D]
] extends Kernel[V, Args]  {
	/**
	 *
	 */
	val kernelArgs: Args
	/**
	 * @return knn
	 */
	def obtainKnn(v: V, env: Seq[V]) = env.sortBy(kernelArgs.metric.d(v, _)).take(kernelArgs.k)
	/**
	 * The KNN kernel for any space, it select KNN using any dissimilarity measure and compute the mode of them by looking for element which minimize average pair to pair distance
	 */
	def obtainMode(v: V, env: GenSeq[V]): V = {
		val knn = obtainKnn(v, env.seq)
		SimilarityMatrix.distanceMinimizer(knn, kernelArgs.metric)
	}
}
/**
 *
 */
trait KnnKernelRealMeta[
	V[Double] <: Seq[Double],
	D <: ContinuousDistance[V[Double]],
	+Args <: KernelArgsKnnRealMeta[V, D]
] extends KnnKernel[Double, V[Double], D, Args]
/**
 *
 */
case class KnnKernelReal[
	V[Double] <: Seq[Double],
	D[V <: Seq[Double]] <: ContinuousDistance[V],
	Args[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]] <: KernelArgsKnnReal[V, D]
](val kernelArgs: Args[V, D]) extends KnnKernelRealMeta[V, D[V[Double]], Args[V, D]]
/**
 *
 */
case class KnnKernelEuclidean[
	V[Double] <: Seq[Double],
	D[V <: Seq[Double]] <: Euclidean[V],
	Args[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]] <: KernelArgsEuclideanKnn[V, D]
](val kernelArgs: Args[V, D]) extends KnnKernelRealMeta[V, D[V[Double]], Args[V, D]] {
	/**
	 * The KNN kernel for euclidean space, it select KNN using a Euclidean measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	override def obtainMode(v: V[Double], env: GenSeq[V[Double]]): V[Double] = {
		val knn = obtainKnn(v, env.seq)
		ClusterBasicOperations.obtainMean(knn)
	}
}