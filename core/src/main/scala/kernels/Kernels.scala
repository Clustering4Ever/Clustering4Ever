package org.clustering4ever.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{exp, tanh, sqrt, Pi, log}
import scala.collection.GenSeq
import breeze.linalg.{DenseVector, DenseMatrix, sum, inv}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations, SimilarityMatrix}
import org.clustering4ever.enums.KernelNature._
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
/**
 * @tparam V
 * @tparam KA the nature of kernel arguments
 */
trait Estimator[V <: GVector[V], KA <: EstimatorArgs] extends Serializable {
	/**
	 * Estimator arguments
	 */
	val kernelArgs: KA
	/**
	 * @return mode given the point and its environment
	 */
	def obtainKernel(v: V, env: GenSeq[V]): V
}
/**
 *
 */
trait EstimatorScalar[KA <: EstimatorArgs] extends Estimator[ScalarVector, KA]
/**
 *
 */
trait EstimatorBinary[KA <: EstimatorArgs] extends Estimator[BinaryVector, KA]
/**
 *
 */
trait EstimatorMixed[KA <: EstimatorArgs] extends Estimator[MixedVector, KA]
/**
 *
 */
object EstimatorUtils {
	/**
	 *
	 */
	final def reducePreModeAndKernelValue(gs: GenSeq[(ScalarVector, Double)]): (ScalarVector, Double) = {
		import org.clustering4ever.util.VectorsAddOperationsImplicits._
		gs.reduce( (a, b) => (SumVectors.sumVectors(a._1, b._1), a._2 + b._2) )
	}
	/**
	 *
	 */
	final def computeModeAndCastIt(preMode: ScalarVector, kernelValue: Double): ScalarVector = ScalarVector(preMode.vector.map(_ / kernelValue))
	/**
	 *
	 */
	final def obtainPreMode(vi: ScalarVector, kernelVal: Double): ScalarVector = ScalarVector(vi.vector.map(_ * kernelVal))
}
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorGaussian[D <: ContinuousDistance](final val kernelArgs: EstimatorArgsGaussian[D]) extends EstimatorScalar[EstimatorArgsGaussian[D]] {
	/** 
	 * Simpliest form of Gaussian kernel as e<sup>(-lambda|x<sub>1</sub>-x<sub>2</sub>|)</sup> where
	 *  - lambda is the bandwidth
	 *  - |x<sub>1</sub>-x<sub>2</sub>| is the distance between x<sub>1</sub> and x<sub>2</sub>
	 */
	final private[this] def gaussianKernel(v1: ScalarVector, altVectors: ScalarVector) = {
		val d = kernelArgs.metric.d(v1, altVectors)
		exp(- kernelArgs.bandwidth * d * d)
	}
	/**
	 *
	 */
	final def obtainKernel(v: ScalarVector, env: GenSeq[ScalarVector]): ScalarVector = {
		val (preMode, kernelValue) = EstimatorUtils.reducePreModeAndKernelValue(
			env.map{ vi =>
				val kernelVal = gaussianKernel(v, vi)
		  		(EstimatorUtils.obtainPreMode(vi, kernelVal), kernelVal)
			}
		)
		EstimatorUtils.computeModeAndCastIt(preMode, kernelValue)
	}
}
/**
 * @tparam V
 */
final case class EstimatorFlat[D <: ContinuousDistance](final val kernelArgs: EstimatorArgsFlat[D]) extends EstimatorScalar[EstimatorArgsFlat[D]] {
	/**
	 *
	 */
	final private[this] def flatKernel(v1: ScalarVector, altVectors: ScalarVector) = {
		val value = kernelArgs.metric.d(v1, altVectors) / (kernelArgs.bandwidth * kernelArgs.bandwidth)
		if( value <= kernelArgs.lambda ) 1D else 0D 
	}
	/**
	 *
	 */
	final def obtainKernel(v: ScalarVector, env: GenSeq[ScalarVector]): ScalarVector = {
		val (preMode, kernelValue) = EstimatorUtils.reducePreModeAndKernelValue(
			env.map{ vi =>
				val kernelVal = flatKernel(v, vi)
		  		(EstimatorUtils.obtainPreMode(vi, kernelVal), kernelVal)
			}
		)
		EstimatorUtils.computeModeAndCastIt(preMode, kernelValue)
	}
}
/**
 * @tparam V
 * @param kernelArgs
 */
final case class EstimatorSigmoid(final val kernelArgs: EstimatorArgsSigmoid) extends EstimatorScalar[EstimatorArgsSigmoid] {
	/**
	 *
	 */
	final private[this] def sigmoidKernel(v1: ScalarVector, v2: ScalarVector) = {
		val dotProduct = SumVectors.dotProduct(v1.vector, v2.vector)
		tanh(kernelArgs.a * dotProduct + kernelArgs.b)
	}
	/**
	 *
	 */
	final def obtainKernel(v: ScalarVector, env: GenSeq[ScalarVector]): ScalarVector = {
		val (preMode, kernelValue) = EstimatorUtils.reducePreModeAndKernelValue(
			env.map{ vi =>
				val kernelVal = sigmoidKernel(v, vi)
		  		(EstimatorUtils.obtainPreMode(vi, kernelVal), kernelVal)
			}
		)
		EstimatorUtils.computeModeAndCastIt(preMode, kernelValue)
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
trait EstimatorKnn[V <: GVector[V], D <: Distance[V], Args <: EstimatorArgsKnn[V, D]] extends Estimator[V, Args] {
	/**
	 * @return knn
	 */
	final private[this] def obtainKnn(v: V, env: Seq[V]) = env.sortBy(kernelArgs.metric.d(v, _)).take(kernelArgs.k)
	/**
	 * The KNN kernel for any space, it select KNN using any dissimilarity measure and compute the mode of them by applying mean, majority vote both or looking for element which minimize average pair to pair distance
	 */
	final def obtainKernel(v: V, env: GenSeq[V]): V = {
		val knn = obtainKnn(v, env.seq)
		ClusterBasicOperations.obtainCenter(knn, kernelArgs.metric)
	}
}
/**
 *
 */
trait EstimatorKnnScalarAncestor[D <: ContinuousDistance, Args <: EstimatorArgsKnnScalarAncestor[D]] extends EstimatorKnn[ScalarVector, D, Args] with EstimatorScalar[Args]
/**
 *
 */
trait EstimatorKnnBinaryAncestor[D <: BinaryDistance, Args <: EstimatorArgsKnnBinaryAncestor[D]] extends EstimatorKnn[BinaryVector, D, Args] with EstimatorBinary[Args]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnScalar[D <: ContinuousDistance](final val kernelArgs: EstimatorArgsKnnScalar[D]) extends EstimatorKnnScalarAncestor[D, EstimatorArgsKnnScalar[D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnBinary[D <: BinaryDistance](final val kernelArgs: EstimatorArgsKnnBinary[D]) extends EstimatorKnnBinaryAncestor[D, EstimatorArgsKnnBinary[D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnEuclidean[D <: Euclidean](final val kernelArgs: EstimatorArgsKnnEuclidean[D]) extends EstimatorKnnScalarAncestor[D, EstimatorArgsKnnEuclidean[D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnEuclideanEasy[D <: Euclidean](k: Int, metric: D) extends EstimatorKnnScalarAncestor[D, EstimatorArgsKnnEuclidean[D]] {
	final val kernelArgs = EstimatorArgsKnnEuclidean(k, metric)
}