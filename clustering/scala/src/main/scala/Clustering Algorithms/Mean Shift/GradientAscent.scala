package org.clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{min, max}
import scala.collection.GenSeq
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.scala.clusterizables.ClusterizableExt
import org.clustering4ever.scala.kernels.{Kernel, KernelArgs}
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 */
class GradientAscent[
  @specialized(Int, Long) ID: Numeric,
  O,
  V <: Seq[Double],
  Cz[ID, O, V] <: ClusterizableExt[ID, O, V, Cz[ID, O, V]],
  D <: ContinuousDistance[V],
  KArgs <: KernelArgs,
  K[X, Y <: KernelArgs] <: Kernel[X, Y]
](
  data: GenSeq[Cz[ID, O, V]],
  epsilon: Double,
  maxIterations: Int,
  kernel: K[V, KArgs],
  metric: D,
  altVectName: Int = Int.MaxValue
) {

  def run(workingVector: Int = 0) = {

    val haveNotConverged = false
    val kernelLocality = data.map(_.vector(workingVector))
    var gradientAscentData: GenSeq[(Cz[ID, O, V], Boolean)] = data.map( obj => (obj.addAltVector(altVectName, obj.vector(workingVector)), haveNotConverged) )

    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, V], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.altVectors(altVectName)
        val updatedMode = if(haveConverged) mode else kernel.obtainMode(mode, kernelLocality)
        val modeShift = metric.d(updatedMode, mode)
        val hasConverged = if(modeShift <= epsilon) {
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

    gradientAscentData.map(_._1)
  }
}
/**
 *
 */
object GradientAscent {
  /**
   * @param data
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   */
  def run[
    ID: Numeric,
    O,
    V <: Seq[Double],
    Cz[ID, O, V] <: ClusterizableExt[ID, O, V, Cz[ID, O, V]],
    D <: ContinuousDistance[V],
    KArgs <: KernelArgs,
    K[X, Y <: KernelArgs] <: Kernel[X, Y]
  ](
    data: GenSeq[Cz[ID, O, V]],
    epsilon: Double,
    maxIterations: Int,
    kernel: K[V, KArgs],
    metric: D,
    altVectName: Int = Int.MaxValue
    )(implicit workingVector: Int = 0) = (new GradientAscent(data, epsilon, maxIterations, kernel, metric, altVectName)).run(workingVector)
}