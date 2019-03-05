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
trait KernelArgs extends Serializable {
	/**
	 * Nature of the Kernel
	 */
	val kernelType: KernelType
}
/**
 * @tparam V
 * @tparam D
 */
trait KernelArgsWithMetric[V <: GVector[V], D <: Distance[V]] extends KernelArgs {
	/**
	 * The metric used by this Kernel
	 */
    val metric: D
}
/**
 * @tparam V
 * @tparam D
 */
trait KernelArgsScalarWithMetric[V <: Seq[Double], D <: ContinuousDistance[V]] extends KernelArgsWithMetric[ScalarVector[V], D]
/**
 * @tparam V
 * @tparam D
 */
trait KernelArgsBinaryWithMetric[V <: Seq[Int], D <: BinaryDistance[V]] extends KernelArgsWithMetric[BinaryVector[V], D]
/**
 *
 */
trait KernelArgsMixedWithMetric[Vb <: Seq[Int], Vs <: Seq[Double], D <: MixedDistance[Vb, Vs]] extends KernelArgsWithMetric[MixedVector[Vb, Vs], D]
/**
 *
 */
trait KernelArgsKnn[V <: GVector[V], D <: Distance[V]] extends KernelArgsWithMetric[V, D] {
	/**
	 * The k value for K Nearest Neighbors search
	 */
    val k: Int
}
/**
 *
 */
trait KernelArgsKnnScalarAncestor[V <: Seq[Double], D <: ContinuousDistance[V]] extends KernelArgsKnn[ScalarVector[V], D] with KernelArgsScalarWithMetric[V, D]
/**
 *
 */
final case class KernelArgsKnnScalar[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val k: Int, final val metric: D[V]) extends KernelArgsKnnScalarAncestor[V, D[V]] {
    final val kernelType = KNN_Real
}
/**
 *
 */
trait KernelArgsKnnBinaryAncestor[V <: Seq[Int], D <: BinaryDistance[V]] extends KernelArgsKnn[BinaryVector[V], D] with KernelArgsBinaryWithMetric[V, D]
/**
 *
 */
final case class KernelArgsKnnBinary[V <: Seq[Int], D[V <: Seq[Int]] <: BinaryDistance[V]](final val k: Int, final val metric: D[V]) extends KernelArgsKnnBinaryAncestor[V, D[V]] {
    final val kernelType = KNN_Binary
}
/**
 *
 */
final case class KernelArgsEuclideanKnn[V <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]](final val k: Int, final val metric: D[V]) extends KernelArgsKnnScalarAncestor[V, D[V]] {
    final val kernelType = KNN_Euclidean
}
/**
 *
 */
final case class EasyKernelArgsEuclideanKnn[V <: Seq[Double]](final val k: Int, squaredRoot: Boolean = true) extends KernelArgsKnnScalarAncestor[V, Euclidean[V]] {
    final val kernelType = KNN_Euclidean

    final val metric = Euclidean[V](squaredRoot)
}
/**
 *
 */
final case class KernelArgsHammingKnn[V <: Seq[Int], D[V <: Seq[Int]] <: Hamming[V]](final val k: Int, final val metric: D[V]) extends KernelArgsKnnBinaryAncestor[V, D[V]] {
    final val kernelType = KNN_Hamming
}
/**
 *
 */
final case class KernelArgsGaussian[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val bandwidth: Double, final val metric: D[V]) extends KernelArgsWithMetric[ScalarVector[V], D[V]] {
    final val kernelType = Gaussian
}
/**
 *
 */  
final case class KernelArgsFlat[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](final val bandwidth: Double, final val metric: D[V], final val lambda: Double = 1D) extends KernelArgsWithMetric[ScalarVector[V], D[V]] {
    final val kernelType = Flat
}
/**
 *
 */
final case class KernelArgsSigmoid(final val a: Double, final val b: Double) extends KernelArgs {
    final val kernelType = Sigmoid
}