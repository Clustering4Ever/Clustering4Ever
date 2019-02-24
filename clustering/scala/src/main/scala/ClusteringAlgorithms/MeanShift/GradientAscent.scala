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
import org.clustering4ever.kernels.{Kernel, KernelArgs}
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
trait GradientAscentAncestor[V <: GVector[V], D <: Distance[V], KArgs <: KernelArgs, K <: Kernel[V, KArgs]] extends GradientAscentArgs[V, D] {
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
      val updatedMode = kernel.obtainMedian(oldMode, kernelLocality)
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
final case class GradientAscent[V <: GVector[V], D[X <: GVector[X]] <: Distance[X], KArgs <: KernelArgs, K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[V, KArgs], final val metric: D[V], final val alternativeVectorID: Int) extends GradientAscentAncestor[V, D[V], KArgs, K[V, KArgs]]
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscentScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], KArgs <: KernelArgs, K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[ScalarVector[V], KArgs], final val metric: D[V], final val alternativeVectorID: Int) extends GradientAscentAncestor[ScalarVector[V], D[V], KArgs, K[ScalarVector[V], KArgs]]
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscentBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X], KArgs <: KernelArgs, K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[BinaryVector[V], KArgs], final val metric: D[V], final val alternativeVectorID: Int) extends GradientAscentAncestor[BinaryVector[V], D[V], KArgs, K[BinaryVector[V], KArgs]]
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 * @param minShift
 * @param maxIterations
 * @param metric
 * @param alternativeVectorID
 */
final case class GradientAscentMixed[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y], KArgs <: KernelArgs, K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y]](final val minShift: Double, final val maxIterations: Int, final val kernel: K[MixedVector[Vb, Vs], KArgs], final val metric: D[Vb, Vs], final val alternativeVectorID: Int) extends GradientAscentAncestor[MixedVector[Vb, Vs], D[Vb, Vs], KArgs, K[MixedVector[Vb, Vs], KArgs]]
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
    V <: Seq[Double],
    Pz[B, C <: GVector[C]] <: Preprocessable[B, C, Pz],
    D[X <: Seq[Double]] <: ContinuousDistance[X],
    KArgs <: KernelArgs,
    K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y],
    GS[X] <: GenSeq[X]
  ](
    data: GS[Pz[O, ScalarVector[V]]],
    minShift: Double,
    maxIterations: Int,
    kernel: K[ScalarVector[V], KArgs],
    metric: D[V],
    alternativeVectorID: Int = Int.MaxValue
  ) = GradientAscentScalar(minShift, maxIterations, kernel, metric, alternativeVectorID).fit(data)
}