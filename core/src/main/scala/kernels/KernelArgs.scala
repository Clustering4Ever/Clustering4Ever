package org.clustering4ever.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.math.distances.scalar.{Euclidean}
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.enums.KernelNature._
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
trait EstimatorArgsScalarWithMetric[V <: Seq[Double], D <: ContinuousDistance[V]] extends EstimatorArgsWithMetric[ScalarVector[V], D]
/**
 * @tparam V
 * @tparam D
 */
trait EstimatorArgsBinaryWithMetric[V <: Seq[Int], D <: BinaryDistance[V]] extends EstimatorArgsWithMetric[BinaryVector[V], D]
/**
 *
 */
trait EstimatorArgsMixedWithMetric[Vb <: Seq[Int], Vs <: Seq[Double], D <: MixedDistance[Vb, Vs]] extends EstimatorArgsWithMetric[MixedVector[Vb, Vs], D]
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
trait EstimatorArgsKnnScalarAncestor[V <: Seq[Double], D <: ContinuousDistance[V]] extends EstimatorArgsKnn[ScalarVector[V], D] with EstimatorArgsScalarWithMetric[V, D]
/**
 *
 */
final case class EstimatorArgsKnnScalar[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val k: Int, final val metric: D[V]) extends EstimatorArgsKnnScalarAncestor[V, D[V]] {
    final val kernelType = KNN_Real
}
/**
 *
 */
trait EstimatorArgsKnnBinaryAncestor[V <: Seq[Int], D <: BinaryDistance[V]] extends EstimatorArgsKnn[BinaryVector[V], D] with EstimatorArgsBinaryWithMetric[V, D]
/**
 *
 */
final case class EstimatorArgsKnnBinary[V <: Seq[Int], D[V <: Seq[Int]] <: BinaryDistance[V]](final val k: Int, final val metric: D[V]) extends EstimatorArgsKnnBinaryAncestor[V, D[V]] {
    final val kernelType = KNN_Binary
}
/**
 *
 */
final case class EstimatorArgsKnnEuclidean[V <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]](final val k: Int, final val metric: D[V]) extends EstimatorArgsKnnScalarAncestor[V, D[V]] {
    final val kernelType = KNN_Euclidean
}
/**
 *
 */
final case class EstimatorArgsKnnHamming[V <: Seq[Int], D[V <: Seq[Int]] <: Hamming[V]](final val k: Int, final val metric: D[V]) extends EstimatorArgsKnnBinaryAncestor[V, D[V]] {
    final val kernelType = KNN_Hamming
}
/**
 *
 */
final case class EstimatorArgsGaussian[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val bandwidth: Double, final val metric: D[V]) extends EstimatorArgsWithMetric[ScalarVector[V], D[V]] {
    final val kernelType = Gaussian
}
/**
 *
 */  
final case class EstimatorArgsFlat[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val bandwidth: Double, final val metric: D[V], final val lambda: Double = 1D) extends EstimatorArgsWithMetric[ScalarVector[V], D[V]] {
    final val kernelType = Flat
}
/**
 *
 */
final case class EstimatorArgsSigmoid(final val a: Double, final val b: Double) extends EstimatorArgs {
    final val kernelType = Sigmoid
}