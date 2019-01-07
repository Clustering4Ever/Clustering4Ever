package org.clustering4ever.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{exp, tanh, sqrt, Pi, log}
import scala.collection.GenSeq
import breeze.linalg.{DenseVector, DenseMatrix, sum, inv}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.{Distance, ContinuousDistance}
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations, SimilarityMatrix}
import org.clustering4ever.enums.KernelNature._
import org.clustering4ever.vectors.{GVector, GBinaryVector, GScalarVector, GMixtVector, ScalarVector}
/**
 *
 */
trait Kernel[V <: GVector[V], KA <: KernelArgs] extends Serializable {
	/**
	 *
	 */
	val kernelArgs: KA
	/**
	 *
	 */
	def obtainMode(v: V, env: GenSeq[V]): V
}
/**
 *
 */
object KernelUtils {
	import org.clustering4ever.util.VectorsAddOperationsImplicits._
	/**
	 *
	 */
	def reducePreModeAndKernelValue[V <: Seq[Double]](gs: GenSeq[(ScalarVector[V], Double)]): (ScalarVector[V], Double) = gs.reduce( (a, b) => (SumVectors.sumVectors(a._1, b._1), a._2 + b._2) )
	/**
	 *
	 */
	def computeModeAndCastIt[V <: Seq[Double]](preMode: ScalarVector[V], kernelValue: Double): ScalarVector[V] = new ScalarVector(preMode.vector.map(_ / kernelValue).asInstanceOf[V])
	/**
	 *
	 */
	def obtainPreMode[V <: Seq[Double]](vi: ScalarVector[V], kernelVal: Double): ScalarVector[V] = new ScalarVector(vi.vector.map(_ * kernelVal).asInstanceOf[V])
}
/**
 *
 */
case class KernelGaussian[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val kernelArgs: KernelArgsGaussian[V, D]) extends Kernel[ScalarVector[V], KernelArgsGaussian[V, D]] {
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	def gaussianKernel(v1: ScalarVector[V], altVectors: ScalarVector[V]) = {
		val d = kernelArgs.metric.d(v1, altVectors)
		exp( - kernelArgs.bandwidth * d * d )
	}
	/**
	 *
	 */
	def obtainMode(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
case class KernelFlat[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val kernelArgs: KernelArgsFlat[V, D]) extends Kernel[ScalarVector[V], KernelArgsFlat[V, D]] {
	/**
	 *
	 */
	def flatKernel(v1: ScalarVector[V], altVectors: ScalarVector[V]) = {
		val value = kernelArgs.metric.d(v1, altVectors) / (kernelArgs.bandwidth * kernelArgs.bandwidth)
		if( value <= kernelArgs.lambda ) 1D else 0D 
	}
	/**
	 *
	 */
	def obtainMode(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
case class KernelSigmoid[V <: Seq[Double]](val kernelArgs: KernelArgsSigmoid) extends Kernel[ScalarVector[V], KernelArgsSigmoid] {
	/**
	 *
	 */
	def sigmoidKernel(v1: ScalarVector[V], altVectors: ScalarVector[V]) = {
		val dotProduct = SumVectors.dotProduct(v1, altVectors)
		tanh(kernelArgs.a * dotProduct + kernelArgs.b)
	}
	/**
	 *
	 */
	def obtainMode(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
	V <: GVector[V],
	D <: Distance[V],
	Args <: KernelArgsKnn[V, D]
] extends Kernel[V, Args] {
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
	V <: Seq[Double],
	D <: ContinuousDistance[V],
	Args <: KernelArgsKnnRealMeta[V, D]
] extends KnnKernel[ScalarVector[V], D, Args]
/**
 *
 */
case class KnnKernelReal[
	V <: Seq[Double],
	D[V <: Seq[Double]] <: ContinuousDistance[V],
	Args[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]] <: KernelArgsKnnReal[V, D]
](val kernelArgs: Args[V, D]) extends KnnKernelRealMeta[V, D[V], Args[V, D]]
/**
 *
 */
case class KnnKernelEuclidean[
	V <: Seq[Double],
	D[V <: Seq[Double]] <: Euclidean[V],
	Args[V <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]] <: KernelArgsEuclideanKnn[V, D]
](val kernelArgs: Args[V, D]) extends KnnKernelRealMeta[V, D[V], Args[V, D]] {
	/**
	 * The KNN kernel for euclidean space, it select KNN using a Euclidean measure and compute the mean<sup>*</sup> of them
	 * @note Mean computation has a sense only for euclidean distance.
	 */
	override def obtainMode(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
		val knn = obtainKnn(v, env.seq)
		ClusterBasicOperations.obtainMean(knn)
	}
}