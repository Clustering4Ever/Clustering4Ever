package clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{min, max}
import scala.collection.GenSeq
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.scala.clusterizables.ClusterizableExt
import clustering4ever.scala.kernels.{Kernel, KernelArgs}
import clustering4ever.scala.basicenum.AlternativeVector
import clustering4ever.scala.basicenum.AlternativeVectorNature._
/**
 * Mean Shift gradient ascent
 * @param kernel defines the nature of kernel and its parameters used in the gradient ascent
 */
class GradientAscent[
  @specialized(Int, Long) ID: Numeric,
  O,
  V <: Seq[Double],
  Cz[ID, O, V <: Seq[Double]] <: ClusterizableExt[ID, O, V, Cz[ID, O, V]],
  D <: ContinuousDistance[V],
  KArgs <: KernelArgs,
  K[V, KArgs <: KernelArgs] <: Kernel[V, KArgs]
](
  data: GenSeq[Cz[ID, O, V]],
  epsilon: Double,
  maxIterations: Int,
  kernel: K[V, KArgs],
  metric: D,
  altVectName: AlternativeVector = Gradient_Ascent
) {

  def run() = {

    val haveNotConverged = false
    val kernelLocality = data.map(_.vector)
    var gradientAscentData: GenSeq[(Cz[ID, O, V], Boolean)] = data.map( obj => (obj.setAltVector(altVectName, obj.vector), haveNotConverged) )

    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, V], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.altVectors.get(altVectName)
        val updatedMode = if( haveConverged ) mode else kernel.obtainMode(mode, kernelLocality)
        val modeShift = metric.d(updatedMode, mode)
        val hasConverged = if( modeShift <= epsilon ) {
          cptConvergedPoints += 1
          true
        }
        else {
          false
        }
        (obj.setAltVector(altVectName, updatedMode), hasConverged)
      }

      (convergingData, cptConvergedPoints)
    }

    var ind = 0
    var everyPointsHaveConverged = false
    while( ind < maxIterations && ! everyPointsHaveConverged ) {
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
    Cz[ID, O, V <: Seq[Double]] <: ClusterizableExt[ID, O, V, Cz[ID, O, V]],
    D <: ContinuousDistance[V],
    KArgs <: KernelArgs,
    K[V, KArgs <: KernelArgs] <: Kernel[V, KArgs]
  ](
    data: GenSeq[Cz[ID, O, V]],
    epsilon: Double,
    maxIterations: Int,
    kernel: K[V, KArgs],
    metric: D,
    altVectName: AlternativeVector = Gradient_Ascent
    ) = (new GradientAscent(data, epsilon, maxIterations, kernel, metric, altVectName)).run()
}