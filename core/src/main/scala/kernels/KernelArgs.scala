package org.clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.{Euclidean}
import org.clustering4ever.math.distances.binary.Hamming
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.scala.vectors.{GVector, ScalarVector, BinaryVector, GMixtVector}
/**
 * ENUM of different kernel types
 */
object KernelNature extends Enumeration {
    type KernelType = Value
    val Flat,
    	KNN,
    	KNN_Real,
    	KNN_Euclidean,
        KNN_Hamming,
    	Gaussian,
    	Sigmoid = Value
}
import org.clustering4ever.scala.kernels.KernelNature._
/**
 * Class regrouping arguments value for a specific kernel type
 */
trait KernelArgs extends Serializable {
	val kernelType: KernelType
}
/**
 *
 */
trait KernelArgsWithMetric[O, D <: Distance[O]] extends KernelArgs {
    val metric: D
}
/**
 *
 */
trait KernelArgsKnn[O, D <: Distance[O]] extends KernelArgsWithMetric[O, D] {
    val k: Int
}
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