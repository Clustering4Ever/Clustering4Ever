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
trait KernelArgsKnnRealMeta[V[Double] <: Seq[Double], D <: ContinuousDistance[V[Double]]] extends KernelArgsKnn[V[Double], D]
/**
 *
 */
trait KernelArgsKnnBinaryMeta[V[Int] <: Seq[Int], D <: BinaryDistance[V[Int]]] extends KernelArgsKnn[V[Int], D]
/**
 *
 */
case class KernelArgsKnnReal[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val k: Int, val metric: D[V[Double]]) extends KernelArgsKnnRealMeta[V, D[V[Double]]] {
    val kernelType = KNN_Real
}
/**
 *
 */
case class KernelArgsEuclideanKnn[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]](val k: Int, val metric: D[V[Double]]) extends KernelArgsKnnRealMeta[V, D[V[Double]]] {
    val kernelType = KNN_Euclidean
}
/**
 *
 */
case class KernelArgsHammingKnn[V[Int] <: Seq[Int], D[V <: Seq[Int]] <: Hamming[V]](val k: Int, val metric: D[V[Int]]) extends KernelArgsKnn[V[Int], D[V[Int]]] {
    val kernelType = KNN_Hamming
}
/**
 *
 */
case class KernelArgsGaussian[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V[Double]]) extends KernelArgsWithMetric[V[Double], D[V[Double]]] {
    val kernelType = Gaussian
}
/**
 *
 */  
case class KernelArgsFlat[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V[Double]], val lambda: Double = 1D) extends KernelArgsWithMetric[V[Double], D[V[Double]]] {
    val kernelType = Flat
}
/**
 *
 */
case class KernelArgsSigmoid(val a: Double, val b: Double) extends KernelArgs {
    val kernelType = Sigmoid
}