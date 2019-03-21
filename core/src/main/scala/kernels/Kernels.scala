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
trait EstimatorScalar[V <: Seq[Double], KA <: EstimatorArgs] extends Estimator[ScalarVector[V], KA]
/**
 *
 */
trait EstimatorBinary[V <: Seq[Int], KA <: EstimatorArgs] extends Estimator[BinaryVector[V], KA]
/**
 *
 */
trait EstimatorMixed[Vb <: Seq[Int], Vs <: Seq[Double], KA <: EstimatorArgs] extends Estimator[MixedVector[Vb, Vs], KA]
/**
 *
 */
object EstimatorUtils {
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
final case class EstimatorGaussian[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val kernelArgs: EstimatorArgsGaussian[V, D]) extends EstimatorScalar[V, EstimatorArgsGaussian[V, D]] {
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
	final def obtainKernel(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
final case class EstimatorFlat[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val kernelArgs: EstimatorArgsFlat[V, D]) extends EstimatorScalar[V, EstimatorArgsFlat[V, D]] {
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
	final def obtainKernel(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
final case class EstimatorSigmoid[V <: Seq[Double]](final val kernelArgs: EstimatorArgsSigmoid) extends EstimatorScalar[V, EstimatorArgsSigmoid] {
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
	final def obtainKernel(v: ScalarVector[V], env: GenSeq[ScalarVector[V]]): ScalarVector[V] = {
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
trait EstimatorKnnScalarAncestor[V <: Seq[Double], D <: ContinuousDistance[V], Args <: EstimatorArgsKnnScalarAncestor[V, D]] extends EstimatorKnn[ScalarVector[V], D, Args] with EstimatorScalar[V, Args]
/**
 *
 */
trait EstimatorKnnBinaryAncestor[V <: Seq[Int], D <: BinaryDistance[V], Args <: EstimatorArgsKnnBinaryAncestor[V, D]] extends EstimatorKnn[BinaryVector[V], D, Args] with EstimatorBinary[V, Args]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnScalar[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val kernelArgs: EstimatorArgsKnnScalar[V, D]) extends EstimatorKnnScalarAncestor[V, D[V], EstimatorArgsKnnScalar[V, D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnBinary[V <: Seq[Int], D[V <: Seq[Int]] <: BinaryDistance[V]](final val kernelArgs: EstimatorArgsKnnBinary[V, D]) extends EstimatorKnnBinaryAncestor[V, D[V], EstimatorArgsKnnBinary[V, D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnEuclidean[V <: Seq[Double], D[X <: Seq[Double]] <: Euclidean[X]](final val kernelArgs: EstimatorArgsKnnEuclidean[V, D]) extends EstimatorKnnScalarAncestor[V, D[V], EstimatorArgsKnnEuclidean[V, D]]
/**
 * @tparam V
 * @tparam D
 */
final case class EstimatorKnnEuclideanEasy[V <: Seq[Double], D[X <: Seq[Double]] <: Euclidean[X]](k: Int, metric: D[V]) extends EstimatorKnnScalarAncestor[V, D[V], EstimatorArgsKnnEuclidean[V, D]] {
	final val kernelArgs = EstimatorArgsKnnEuclidean(k, metric)
}