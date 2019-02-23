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
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
/**
 * @tparam V
 * @tparam KA the nature of kernel arguments
 */
trait Kernel[V <: GVector[V], KA <: KernelArgs] extends Serializable {
	/**
	 * Kernel arguments
	 */
	val kernelArgs: KA
	/**
	 * @return mode given the point and its environment
	 */
	def obtainMedian(v: V, env: GenSeq[V]): V
}
/**
 *
 */
trait KernelScalar[V <: Seq[Double], KA <: KernelArgs] extends Kernel[ScalarVector[V], KA]
/**
 *
 */
trait KernelBinary[V <: Seq[Int], KA <: KernelArgs] extends Kernel[BinaryVector[V], KA]
/**
 *
 */
trait KernelMixt[Vb <: Seq[Int], Vs <: Seq[Double], KA <: KernelArgs] extends Kernel[MixedVector[Vb, Vs], KA]
/**
 *
 */
object KernelUtils {
	/**
	 *
	 */
	final def reducePreModeAndKernelValue[V <: Seq[Double]](gs: GenSeq[(ScalarVector[V], Double)]): (ScalarVector[V], Double) = {
		import org.clustering4ever.util.VectorsAddOperationsImplicits._
		gs.reduce( (a, b) => (SumVectors.sumVectors(a._1, b._1), a._2 + b._2) )
	}
	/**
	 *
	 */
	final def computeModeAndCastIt[V <: Seq[Double]](preMode: ScalarVector[V], kernelValue: Double): ScalarVector[V] = ScalarVector(preMode.vector.map(_ / kernelValue).asInstanceOf[V])
	/**
	 *
	 */
	final def obtainPreMode[V <: Seq[Double]](vi: ScalarVector[V], kernelVal: Double): ScalarVector[V] = ScalarVector(vi.vector.map(_ * kernelVal).asInstanceOf[V])
}
/**
 * @tparam V
 * @tparam D
 */
final case class GaussianKernel[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val kernelArgs: KernelArgsGaussian[V, D]) extends KernelScalar[V, KernelArgsGaussian[V, D]] {
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	final private[this] def gaussianKernel(v1: ScalarVector[V], altVectors: ScalarVector[V]) = {
		val d = kernelArgs.metric.d(v1, altVectors)
		exp(- kernelArgs.bandwidth * d * d)
	}
	/**
	 *
	 */
	final def obtainMedian(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
 * @tparam V
 */
final case class FlatKernel[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val kernelArgs: KernelArgsFlat[V, D]) extends KernelScalar[V, KernelArgsFlat[V, D]] {
	/**
	 *
	 */
	final private[this] def flatKernel(v1: ScalarVector[V], altVectors: ScalarVector[V]) = {
		val value = kernelArgs.metric.d(v1, altVectors) / (kernelArgs.bandwidth * kernelArgs.bandwidth)
		if( value <= kernelArgs.lambda ) 1D else 0D 
	}
	/**
	 *
	 */
	final def obtainMedian(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
 * @tparam V
 * @param kernelArgs
 */
final case class SigmoidKernel[V <: Seq[Double]](final val kernelArgs: KernelArgsSigmoid) extends KernelScalar[V, KernelArgsSigmoid] {
	/**
	 *
	 */
	final private[this] def sigmoidKernel(v1: ScalarVector[V], v2: ScalarVector[V]) = {
		val dotProduct = SumVectors.dotProduct(v1, v2)
		tanh(kernelArgs.a * dotProduct + kernelArgs.b)
	}
	/**
	 *
	 */
	final def obtainMedian(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
 */
object GmmKernels {
	/**
	 *
	 */
	final def multivariateGaussianKernelLog(v1: DenseVector[Double], mean: DenseVector[Double], invSigma: DenseMatrix[Double]): Double = {
		val diff = v1 - mean
		- diff.t * invSigma * diff * 0.5
	}
	/**
	 *
	 */
	final def multivariateGaussianKernel(v1: DenseVector[Double], mean: DenseVector[Double], invSigma: DenseMatrix[Double]): Double = {
		exp(multivariateGaussianKernelLog(v1, mean, invSigma))
	}
}
/**
 * @tparam V
 * @tparam D
 * @tparam Args
 */
trait KnnKernel[V <: GVector[V], D <: Distance[V], Args <: KernelArgsKnn[V, D]] extends Kernel[V, Args] {
	/**
	 * @return knn
	 */
	final private[this] def obtainKnn(v: V, env: Seq[V]) = env.sortBy(kernelArgs.metric.d(v, _)).take(kernelArgs.k)
	/**
	 * The KNN kernel for any space, it select KNN using any dissimilarity measure and compute the mode of them by applying mean, majority vote both or looking for element which minimize average pair to pair distance
	 */
	final def obtainMedian(v: V, env: GenSeq[V]): V = {
		val knn = obtainKnn(v, env.seq)
		ClusterBasicOperations.obtainMinimizingPoint(knn, kernelArgs.metric)
	}
}
/**
 *
 */
trait KnnKernelRealMeta[V <: Seq[Double], D <: ContinuousDistance[V], Args <: KernelArgsKnnScalar[V, D]] extends KnnKernel[ScalarVector[V], D, Args] with KernelScalar[V, Args]
/**
 * @tparam V
 * @tparam D
 */
final case class KnnKernelReal[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val kernelArgs: KernelArgsKnnReal[V, D]) extends KnnKernelRealMeta[V, D[V], KernelArgsKnnReal[V, D]]
/**
 * @tparam V
 * @tparam D
 */
final case class KnnKernelEuclidean[V <: Seq[Double], D[X <: Seq[Double]] <: Euclidean[X]](final val kernelArgs: KernelArgsEuclideanKnn[V, D]) extends KnnKernelRealMeta[V, D[V], KernelArgsEuclideanKnn[V, D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EasyKnnKernelEuclidean[V <: Seq[Double], D[X <: Seq[Double]] <: Euclidean[X]](k: Int, metric: D[V]) extends KnnKernelRealMeta[V, D[V], KernelArgsEuclideanKnn[V, D]] {
	final val kernelArgs = KernelArgsEuclideanKnn(k, metric)
}
/**
 * @tparam V
 */
final case class SuperEasyKnnKernelEuclidean[V <: Seq[Double]](k: Int, squaredRoot: Boolean = true) extends KnnKernelRealMeta[V, Euclidean[V], KernelArgsEuclideanKnn[V, Euclidean]] {
	final val kernelArgs = KernelArgsEuclideanKnn(k, Euclidean[V](squaredRoot))
}
/**
 * @tparam V
 */
final case class HyperEasyKnnKernelEuclidean[V <: Seq[Double]](k: Int) extends KnnKernelRealMeta[V, Euclidean[V], KernelArgsEuclideanKnn[V, Euclidean]] {
	final val kernelArgs = KernelArgsEuclideanKnn(k, Euclidean[V](true))
}