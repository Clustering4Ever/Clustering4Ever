package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import clustering4ever.math.distances.{Distance, ContinuousDistance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.scalar.Euclidean
/**
 * ENUM of different kernel types
 */
object KernelNature extends Enumeration {
    type KernelType = Value
    val Flat,
    	KNN,
    	KNN_Real,
    	KNN_Binary,
    	EuclideanKNN,
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
abstract class KernelArgsWithMetric[O, D <: Distance[O]](val metric: D) extends KernelArgs
/**
 *
 */
class KernelArgsKNN[O, D <: Distance[O]](val k: Int, metric: D) extends KernelArgsWithMetric[O, D](metric) {
    val kernelType = KNN
}
/**
 *
 */
class KernelArgsKNNReal[V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]](val k: Int, metric: D[V]) extends KernelArgsWithMetric[V, D[V]](metric) {
    val kernelType = KNN_Real
}
/**
 *
 */
class KernelArgsEuclideanKNN[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: Euclidean[V]](val k: Int, metric: D[V]) extends KernelArgsWithMetric[V[Double], D[V]](metric) {
    val kernelType = EuclideanKNN
}
/**
 *
 */
class KernelArgsGaussian[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]](val bandwidth: Double, metric: D[V]) extends KernelArgsWithMetric[V[Double], D[V]](metric) {
    val kernelType = Gaussian
}
/**
 *
 */  
class KernelArgsFlat[V[Double] <: Seq[Double], D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]](val bandwidth: Double, metric: D[V], val lambda: Double = 1D) extends KernelArgsWithMetric[V[Double], D[V]](metric) {
    val kernelType = Flat
}
/**
 *
 */
class KernelArgsSigmoid(val a: Double, val b: Double) extends KernelArgs {
    val kernelType = Sigmoid
}