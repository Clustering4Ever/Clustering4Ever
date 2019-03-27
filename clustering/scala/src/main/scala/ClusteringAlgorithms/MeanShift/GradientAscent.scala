package org.clustering4ever.clustering.scala.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{min, max}
import scala.collection.GenSeq
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.kernels.{Estimator, EstimatorArgs}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.preprocessing.PreprocessingArgs
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.arguments.{MinShiftArgs, MaxIterationsArgs, MetricArgs}
/**
 *
 */
trait GradientAscentArgs[V <: GVector[V], D <: Distance[V]] extends MinShiftArgs with MaxIterationsArgs with MetricArgs[V, D] 
/**
 *
 */
trait GradientAscentAncestor[V <: GVector[V], D <: Distance[V], KArgs <: EstimatorArgs, K <: Estimator[V, KArgs]] extends GradientAscentArgs[V, D] {
  /**
   * The threshold under which points are considered stationary
   */
  val minShift: Double
  /**
   * The maximum iterations number allowed for this algorithm 
   */
  val maxIterations: Int
  /**
   * The kernel used
   */
  val kernel: K
  /**
   * The metric used to measure the shift distance of the mode at each iteration 
   */
  val metric: D
  /**
   * The ID where the mode is stored
   */
  val alternativeVectorID: Int
  /**
   * @tparam O the raw object nature
   * @tparam Pz a descendant of preprocessable
   * @tparam GS the nature of the collection
   * The main method to run the gradient ascent
   */
  final def fit[O, Pz[Y, Z <: GVector[Z]] <: Preprocessable[Y, Z, Pz], GS[X] <: GenSeq[X]](data: GS[Pz[O, V]]): GS[(Pz[O, V], Double)] = {

    val kernelLocality = data.map(_.v)

    @annotation.tailrec
    def obtainFinalMode(i: Int, oldMode: V, shift: Double): (V, Double) = {
      val updatedMode = kernel.obtainKernel(oldMode, kernelLocality)
      val minShiftShift = metric.d(oldMode, updatedMode)
      val updatedShift = shift + minShiftShift
      if(i < maxIterations && minShiftShift > minShift) obtainFinalMode(i + 1, updatedMode, updatedShift)
      else (updatedMode, updatedShift)
    }

    data.map{ pz =>
      val (mode, shiftSum) = obtainFinalMode(0, pz.v, 0D)
      val pzWithMode = pz.addAlternativeVector(alternativeVectorID, mode)
      (pzWithMode, shiftSum)
    }.asInstanceOf[GS[(Pz[O, V], Double)]]
  }

}
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscent[V <: GVector[V], D[X <: GVector[X]] <: Distance[X], KArgs <: EstimatorArgs, K[X <: GVector[X], Y <: EstimatorArgs] <: Estimator[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[V, KArgs], final val metric: D[V], final val alternativeVectorID: Int) extends GradientAscentAncestor[V, D[V], KArgs, K[V, KArgs]]
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscentScalar[D <: ContinuousDistance, KArgs <: EstimatorArgs, K[X <: GVector[X], Y <: EstimatorArgs] <: Estimator[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[ScalarVector, KArgs], final val metric: D, final val alternativeVectorID: Int) extends GradientAscentAncestor[ScalarVector, D, KArgs, K[ScalarVector, KArgs]]
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscentBinary[D <: BinaryDistance, KArgs <: EstimatorArgs, K[X <: GVector[X], Y <: EstimatorArgs] <: Estimator[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[BinaryVector, KArgs], final val metric: D, final val alternativeVectorID: Int) extends GradientAscentAncestor[BinaryVector, D, KArgs, K[BinaryVector, KArgs]]
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscentMixed[D <: MixedDistance, KArgs <: EstimatorArgs, K[X <: GVector[X], Y <: EstimatorArgs] <: Estimator[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[MixedVector, KArgs], final val metric: D, final val alternativeVectorID: Int) extends GradientAscentAncestor[MixedVector, D, KArgs, K[MixedVector, KArgs]]
/**
 *
 */
object GradientAscentScalar {
  /**
   * @param data GenSeq of Pz[O, ScalarVector[V]]
   * @param minShift threshold under which we stop iteration in gradient ascent
   * @param maxIterations Number of iteration for modes search
   */
  final def fit[
    O,
    Pz[B, C <: GVector[C]] <: Preprocessable[B, C, Pz],
    D <: ContinuousDistance,
    KArgs <: EstimatorArgs,
    K[X <: GVector[X], Y <: EstimatorArgs] <: Estimator[X, Y],
    GS[X] <: GenSeq[X]
  ](
    data: GS[Pz[O, ScalarVector]],
    minShift: Double,
    maxIterations: Int,
    kernel: K[ScalarVector, KArgs],
    metric: D,
    alternativeVectorID: Int = Int.MaxValue
  ) = GradientAscentScalar(minShift, maxIterations, kernel, metric, alternativeVectorID).fit(data)
}