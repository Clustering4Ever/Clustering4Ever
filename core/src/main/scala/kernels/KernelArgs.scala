package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import clustering4ever.math.distances.{Distance, ContinuousDistance, DistanceSeq}
import clustering4ever.math.distances.scalar.{Euclidean}
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
trait KernelArgsWithMetricSeq[@specialized(Int, Double) N, V <: Seq[N], D <: DistanceSeq[N, V]] extends KernelArgsWithMetric[V, D] {
    val metric: D
}
/**
 * Investigate why specialization with Spire Numeric prevent compilations
 */
trait KernelArgsKnnVec[@specialized(Int, Double) N, V <: Seq[N], D <: DistanceSeq[N, V]] extends KernelArgsKnn[V, D] {
    val metric: D
    val k: Int
}
/**
 *
 */
trait KernelArgsKnnRealMeta[V <: Seq[Double], D <: ContinuousDistance[V]] extends KernelArgsKnnVec[Double, V, D]
/**
 *
 */
class KernelArgsKnnReal[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val k: Int, val metric: D[V[Double]]) extends KernelArgsKnnRealMeta[V[Double], D[V[Double]]] {
    val kernelType = KNN_Real
}
/**
 *
 */
class KernelArgsEuclideanKnn[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: Euclidean[V]](val k: Int, val metric: D[V[Double]]) extends KernelArgsKnnRealMeta[V[Double], D[V[Double]]] {
    val kernelType = KNN_Euclidean
}
/**
 *
 */
class KernelArgsHammingKnn[V[Int] <: Seq[Int], D[V <: Seq[Int]] <: Hamming[V]](val k: Int, val metric: D[V[Int]]) extends KernelArgsKnnVec[Int, V[Int], D[V[Int]]] {
    val kernelType = KNN_Hamming
}
/**
 *
 */
class KernelArgsGaussian[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V[Double]]) extends KernelArgsWithMetricSeq[Double, V[Double], D[V[Double]]] {
    val kernelType = Gaussian
}
/**
 *
 */  
class KernelArgsFlat[V[Double] <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val bandwidth: Double, val metric: D[V[Double]], val lambda: Double = 1D) extends KernelArgsWithMetricSeq[Double, V[Double], D[V[Double]]] {
    val kernelType = Flat
}
/**
 *
 */
class KernelArgsSigmoid(val a: Double, val b: Double) extends KernelArgs {
    val kernelType = Sigmoid
}