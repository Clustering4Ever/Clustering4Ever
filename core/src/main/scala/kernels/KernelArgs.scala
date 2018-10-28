package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import clustering4ever.math.distances.{Distance, ContinuousDistance, DistanceSeq}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.binary.Hamming
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
import clustering4ever.scala.kernels.KernelNature._
/**
 * Class regrouping arguments value for a specific kernel type
 */
trait KernelArgs {
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
trait KernelArgsWithMetricSeq[N, V[N] <: Seq[N], D <: DistanceSeq[N, V]] extends KernelArgsWithMetric[V[N], D] {
    val metric: D
}
/**
 * Investigate why specialization with Spire Numeric prevent compilations
 */
trait KernelArgsKnnVec[N, V[N] <: Seq[N], D <: DistanceSeq[N, V]] extends KernelArgsWithMetricSeq[N, V, D] {
    val metric: D
    val k: Int
}
/**
 *
 */
trait KernelArgsKnnRealMeta[V[Double] <: Seq[Double], D <: ContinuousDistance[V]] extends KernelArgsKnnVec[Double, V, D] {
    val k: Int
    val metric: D
}
/**
 *
 */
class KernelArgsKnnReal[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V]](val k: Int, val metric: D[V]) extends KernelArgsKnnRealMeta[V, D[V]] {
    val kernelType = KNN_Real
}
/**
 *
 */
class KernelArgsEuclideanKnn[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: Euclidean[V]](val k: Int, val metric: D[V]) extends KernelArgsKnnRealMeta[V, D[V]] {
    val kernelType = KNN_Euclidean
}
/**
 *
 */
class KernelArgsHammingKnn[V[Int] <: Seq[Int], D[V[Int] <: Seq[Int]] <: Hamming[V]](val k: Int, val metric: D[V]) extends KernelArgsKnnVec[Int, V, D[V]] {
    val kernelType = KNN_Hamming
}
/**
 *
 */
class KernelArgsGaussian[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V]) extends KernelArgsWithMetricSeq[Double, V, D[V]] {
    val kernelType = Gaussian
}
/**
 *
 */  
class KernelArgsFlat[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V], val lambda: Double = 1D) extends KernelArgsWithMetricSeq[Double, V, D[V]] {
    val kernelType = Flat
}
/**
 *
 */
class KernelArgsSigmoid(val a: Double, val b: Double) extends KernelArgs {
    val kernelType = Sigmoid
}