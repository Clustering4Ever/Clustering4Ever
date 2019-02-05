package org.clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{min, max}
import scala.collection.GenSeq
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.kernels.{Kernel, KernelArgs}
import org.clustering4ever.vectors.{GVector, ScalarVector}
import org.clustering4ever.preprocessing.PreprocessingArgs
/**
 *
 */
case class GradientAscentArgs[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], KArgs <: KernelArgs, K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y]](val epsilon: Double, val maxIterations: Int, val kernel: K[ScalarVector[V], KArgs], val metric: D[V], val alternativeVectorID: Int) extends PreprocessingArgs {
  val algorithm = org.clustering4ever.extensibleAlgorithmNature.GradientAscent
}
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 */
case class GradientAscent[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], KArgs <: KernelArgs, K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y], GS[X] <: GenSeq[X]](args: GradientAscentArgs[V, D, KArgs, K]) {
  /**
   *
   */
  def run[O, Pz[Y, Z <: GVector[Z]] <: Preprocessable[Y, Z, Pz]](data: GS[Pz[O, ScalarVector[V]]]): GS[Pz[O, ScalarVector[V]]] = {

    val haveNotConverged = false
    val kernelLocality = data.map(_.v)
    val gradientAscentData: GenSeq[(Pz[O, ScalarVector[V]], Boolean)] = data.map( obj => (obj.addAlternativeVector(args.alternativeVectorID, obj.v), haveNotConverged) )

    def kernelGradientAscent(toExplore: GenSeq[(Pz[O, ScalarVector[V]], Boolean)]): (GenSeq[(Pz[O, ScalarVector[V]], Boolean)], Int) = {
      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.vectorized.get(args.alternativeVectorID)(VMapping[Int, ScalarVector[V]]).get
        val updatedMode = if(haveConverged) mode else args.kernel.obtainMode(mode, kernelLocality)
        val modeShift = args.metric.d(updatedMode, mode)
        val hasConverged = if(modeShift <= args.epsilon) {
            cptConvergedPoints += 1
            true
          }
          else false
        (obj.addAlternativeVector(args.alternativeVectorID, updatedMode), hasConverged)
      }
      (convergingData, cptConvergedPoints)
    }

    @annotation.tailrec
    def go(i: Int, data: GenSeq[(Pz[O, ScalarVector[V]], Boolean)]): GenSeq[(Pz[O, ScalarVector[V]], Boolean)] = {
      val (ascendedData, cptConvergedPoints) = kernelGradientAscent(data)
      val everyPointsHaveConverged = cptConvergedPoints == gradientAscentData.size
      if(i < args.maxIterations && !everyPointsHaveConverged) go(i + 1, ascendedData)
      else ascendedData
    }

    go(0, gradientAscentData).map(_._1).asInstanceOf[GS[Pz[O, ScalarVector[V]]]]

  }
}
/**
 *
 */
object GradientAscent {
  /**
   * @param data GenSeq of Pz[O, ScalarVector[V]]
   * @param epsilon threshold under which we stop iteration in gradient ascent
   * @param maxIterations Number of iteration for modes search
   */
  def run[
    O,
    V <: Seq[Double],
    Pz[B, C <: GVector[C]] <: Preprocessable[B, C, Pz],
    D[X <: Seq[Double]] <: ContinuousDistance[X],
    KArgs <: KernelArgs,
    K[X <: GVector[X], Y <: KernelArgs] <: Kernel[X, Y],
    GS[X] <: GenSeq[X]
  ](
    data: GS[Pz[O, ScalarVector[V]]],
    epsilon: Double,
    maxIterations: Int,
    kernel: K[ScalarVector[V], KArgs],
    metric: D[V],
    alternativeVectorID: Int = Int.MaxValue
  ) = (GradientAscent(GradientAscentArgs(epsilon, maxIterations, kernel, metric, alternativeVectorID))).run(data)
}