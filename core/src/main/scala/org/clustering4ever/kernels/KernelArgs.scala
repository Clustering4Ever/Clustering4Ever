package org.clustering4ever.kernels
/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.binary.Hamming
import org.clustering4ever.distances.continuous.Euclidean
import org.clustering4ever.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.roottraits.KernelNature._
import org.clustering4ever.roottraits.{BinaryVector, GVector, MixedVector, ScalarVector}

import scala.language.higherKinds
/**
 * Class regrouping arguments value for a specific kernel type
 */
trait EstimatorArgs extends Serializable {
	/**
	 * Nature of the Kernel
	 */
	val kernelType: KernelType
}
/**
 * @tparam V
 * @tparam D
 */
trait EstimatorArgsWithMetric[V <: GVector[V], D <: Distance[V]] extends EstimatorArgs {
	/**
	 * The metric used by this Kernel
	 */
    val metric: D
}
/**
 * @tparam V
 * @tparam D
 */
trait EstimatorArgsScalarWithMetric[D <: ContinuousDistance] extends EstimatorArgsWithMetric[ScalarVector, D]
/**
 * @tparam V
 * @tparam D
 */
trait EstimatorArgsBinaryWithMetric[D <: BinaryDistance] extends EstimatorArgsWithMetric[BinaryVector, D]
/**
 *
 */
trait EstimatorArgsMixedWithMetric[D <: MixedDistance] extends EstimatorArgsWithMetric[MixedVector, D]
/**
 *
 */
trait EstimatorArgsKnn[V <: GVector[V], D <: Distance[V]] extends EstimatorArgsWithMetric[V, D] {
	/**
	 * The k value for K Nearest Neighbors search
	 */
    val k: Int
}
/**
 *
 */
trait EstimatorArgsKnnScalarAncestor[D <: ContinuousDistance] extends EstimatorArgsKnn[ScalarVector, D] with EstimatorArgsScalarWithMetric[D]
/**
 *
 */
final case class EstimatorArgsKnnScalar[D <: ContinuousDistance](final val k: Int, final val metric: D) extends EstimatorArgsKnnScalarAncestor[D] {
    final val kernelType = KNN_Real
}
/**
 *
 */
trait EstimatorArgsKnnBinaryAncestor[D <: BinaryDistance] extends EstimatorArgsKnn[BinaryVector, D] with EstimatorArgsBinaryWithMetric[D]
/**
 *
 */
final case class EstimatorArgsKnnBinary[D <: BinaryDistance](final val k: Int, final val metric: D) extends EstimatorArgsKnnBinaryAncestor[D] {
    final val kernelType = KNN_Binary
}
/**
 *
 */
final case class EstimatorArgsKnnEuclidean[D <: Euclidean](final val k: Int, final val metric: D) extends EstimatorArgsKnnScalarAncestor[D] {
    final val kernelType = KNN_Euclidean
}
/**
 *
 */
final case class EstimatorArgsKnnHamming[D <: Hamming](final val k: Int, final val metric: D) extends EstimatorArgsKnnBinaryAncestor[D] {
    final val kernelType = KNN_Hamming
}
/**
 *
 */
final case class EstimatorArgsGaussian[D <: ContinuousDistance](final val bandwidth: Double, final val metric: D) extends EstimatorArgsWithMetric[ScalarVector, D] {
    final val kernelType = Gaussian
}
/**
 *
 */  
final case class EstimatorArgsFlat[D <: ContinuousDistance](final val bandwidth: Double, final val metric: D, final val lambda: Double = 1D) extends EstimatorArgsWithMetric[ScalarVector, D] {
    final val kernelType = Flat
}
/**
 *
 */
final case class EstimatorArgsSigmoid(final val a: Double, final val b: Double) extends EstimatorArgs {
    final val kernelType = Sigmoid
}