package clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Random
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumVectors
import clustering4ever.scala.kernels.KernelNature._
import clustering4ever.scala.kernels.KernelNature
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.kernels.{Kernel, KernelSeq, KernelArgs}//, KernelArgsKnnVec, KnnKernelReal}
/**
 * Mean Shift gradient ascent
 * @param kernelArgs defines the nature of kernel and its parameters used in the gradient ascent, provide a Left[KernelArgs] or Right[KernelArgs] depending on this fact
 *  * Right[KernelArgs] if KNN or EuclideanKNN kernel
 *  * Left[KernelArgs] else
 */
class GradientAscent[
  @specialized(Int, Long) ID: Numeric,
  O,
  V[Double] <: Seq[Double],
  Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
  D <: ContinuousDistance[V[Double]],
  K <: Kernel[V[Double], KernelArgs]
](
  data: GenSeq[Cz[ID, O, V[Double]]],
  epsilon: Double,
  maxIterations: Int,
  kernel: K,
  metric: D
) {

  def run() = {

    val haveNotConverged = false
    val kernelLocality = data.map(_.vector)
    var gradientAscentData: GenSeq[(Cz[ID, O, V[Double]], Boolean)] = data.map( obj => (obj.setV2(obj.vector), haveNotConverged) )

    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, V[Double]], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.v2.get
        val updatedMode = if( haveConverged ) mode else kernel.obtainMode(mode, kernelLocality)
        val modeShift = metric.d(updatedMode, mode)
        val hasConverged = if( modeShift <= epsilon ) {
          cptConvergedPoints += 1
          true
        }
        else {
          false
        }
        (obj.setV2(updatedMode), hasConverged)
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
    V[Double] <: Seq[Double],
    Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
    D <: ContinuousDistance[V[Double]],
    K <: Kernel[V[Double], KernelArgs]
  ](
    data: GenSeq[Cz[ID, O, V[Double]]],
    epsilon: Double,
    maxIterations: Int,
    kernel: K,
    metric: D
    ) = (new GradientAscent(data, epsilon, maxIterations, kernel, metric)).run()
}