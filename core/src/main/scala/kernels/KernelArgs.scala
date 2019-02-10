package org.clustering4ever.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.{Euclidean}
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, GMixedVector}
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
 *
 */
trait GenericKernelArgsWithMetric[O, D <: GenericDistance[O]] extends KernelArgs {
	/**
	 * The metric used by this Kernel
	 */
    val metric: D
}
/**
 *
 */
trait GenericKernelArgsKnn[O, D <: GenericDistance[O]] extends GenericKernelArgsWithMetric[O, D] {
	/**
	 * The k value for KNN
	 */
    val k: Int
}
/**
 *
 */
trait KernelArgsWithMetric[V <: GVector[V], D <: Distance[V]] extends GenericKernelArgsWithMetric[V, D]
/**
 *
 */
trait KernelArgsKnn[V <: GVector[V], D <: Distance[V]] extends GenericKernelArgsKnn[V, D]
/**
 *
 */
trait KernelArgsKnnRealMeta[V <: Seq[Double], D <: ContinuousDistance[V]] extends KernelArgsKnn[ScalarVector[V], D]
/**
 *
 */
trait KernelArgsKnnBinaryMeta[V <: Seq[Int], D <: BinaryDistance[V]] extends KernelArgsKnn[BinaryVector[V], D]
/**
 *
 */
case class KernelArgsKnnReal[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val k: Int, val metric: D[V]) extends KernelArgsKnnRealMeta[V, D[V]] {
    val kernelType = KNN_Real
}
/**
 *
 */
case class KernelArgsEuclideanKnn[V <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]](val k: Int, val metric: D[V]) extends KernelArgsKnnRealMeta[V, D[V]] {
    val kernelType = KNN_Euclidean
}
/**
 *
 */
case class KernelArgsHammingKnn[V <: Seq[Int], D[V <: Seq[Int]] <: Hamming[V]](val k: Int, val metric: D[V]) extends KernelArgsKnn[BinaryVector[V], D[V]] {
    val kernelType = KNN_Hamming
}
/**
 *
 */
case class KernelArgsGaussian[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V]) extends KernelArgsWithMetric[ScalarVector[V], D[V]] {
    val kernelType = Gaussian
}
/**
 *
 */  
case class KernelArgsFlat[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V], val lambda: Double = 1D) extends KernelArgsWithMetric[ScalarVector[V], D[V]] {
    val kernelType = Flat
}
/**
 *
 */
case class KernelArgsSigmoid(val a: Double, val b: Double) extends KernelArgs {
    val kernelType = Sigmoid
}