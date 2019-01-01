package org.clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{min, max}
import scala.collection.GenSeq
import org.clustering4ever.shapelesslinked.VMapping
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.scala.kernels.{Kernel, KernelArgs}
import org.clustering4ever.scala.vectors.{GVector, ScalarVector}
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 */
class GradientAscent[
  @specialized(Int, Long) ID: Numeric,
  O,
  V <: Seq[Double],
  Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
  D <: ContinuousDistance[V],
  KArgs <: KernelArgs,
  K[X <: GVector, Y <: KernelArgs] <: Kernel[X, Y],
  GS[X] <: GenSeq[X]
](
  data: GS[Cz[ID, O, ScalarVector[V]]],
  epsilon: Double,
  maxIterations: Int,
  kernel: K[ScalarVector[V], KArgs],
  metric: D,
  altVectName: Int = Int.MaxValue
) {

  def run() = {

    implicit val vMapping = new VMapping[Int, ScalarVector[V]]

    val haveNotConverged = false
    val kernelLocality = data.map(_.workingVector)
    var gradientAscentData: GenSeq[(Cz[ID, O, ScalarVector[V]], Boolean)] = data.map( obj => (obj.addAltVector(altVectName, obj.workingVector), haveNotConverged) )

    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, ScalarVector[V]], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.vectorized.get(altVectName).get
        val updatedMode = if(haveConverged) mode else kernel.obtainMode(mode, kernelLocality)
        val modeShift = metric.d(updatedMode, mode)
        val hasConverged = 
          if(modeShift <= epsilon) {
            cptConvergedPoints += 1
            true
          }
          else {
            false
          }
        (obj.addAltVector(altVectName, updatedMode), hasConverged)
      }

      (convergingData, cptConvergedPoints)
    }

    var ind = 0
    var everyPointsHaveConverged = false
    while(ind < maxIterations && !everyPointsHaveConverged) {
      val (gradientAscentData0, cptConvergedPoints) = kernelGradientAscent(gradientAscentData)
      gradientAscentData = gradientAscentData0
      everyPointsHaveConverged = cptConvergedPoints == gradientAscentData.size
      ind += 1
    }

    gradientAscentData.map(_._1).asInstanceOf[GS[Cz[ID, O, ScalarVector[V]]]]
  }
}
/**
 *
 */
object GradientAscent {
  /**
   * @param dataCz[ID, O, ScalarVector[V]]
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   */
  def run[
    ID: Numeric,
    O,
    V <: Seq[Double],
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
    D <: ContinuousDistance[V],
    KArgs <: KernelArgs,
    K[X <: GVector, Y <: KernelArgs] <: Kernel[X, Y],
    GS[X] <: GenSeq[X]
  ](
    data: GS[Cz[ID, O, ScalarVector[V]]],
    epsilon: Double,
    maxIterations: Int,
    kernel: K[ScalarVector[V], KArgs],
    metric: D,
    altVectName: Int = Int.MaxValue
    ) = (new GradientAscent(data, epsilon, maxIterations, kernel, metric, altVectName)).run()
}