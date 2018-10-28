package clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.util.Random
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumVectors
import clustering4ever.scala.kernels.{Kernels, KernelArgs}
import clustering4ever.scala.kernels.KernelNature._
import clustering4ever.scala.kernels.KernelNature
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.kernels.KernelNature.EuclideanKNN
import scala.language.higherKinds
/**
 * Mean Shift gradient ascent
 * @param kernelArgs defines the nature of kernel and its parameters used in the gradient ascent, provide a Left[KernelArgs] or Right[KernelArgs] depending on this fact
 *  * Right[KernelArgs] if KNN or EuclideanKNN kernel
 *  * Left[KernelArgs] else
 */
class GradientAscent[
  ID: Numeric,
  O,
  V[Double] <: Seq[Double],
  Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
  D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]
](
  data: GenSeq[Cz[ID, O, V[Double]]],
  epsilon: Double,
  maxIterations: Int,
  metric: D[V] = new Euclidean[V](true),
  kernelArgs: KernelArgs
) {

  def run() = {

    val haveNotConverged = false
    val kernelLocality = data.map(_.vector)
    var gradientAscentData: GenSeq[(Cz[ID, O, V[Double]], Boolean)] = data.map( obj => (obj.setV2(obj.vector), haveNotConverged) )
    lazy val kernelLocalitySeq: Seq[V[Double]] = kernelLocality.seq
    

    val (kernelNature, kernel) = kernelArgs.kernelType match {
      case KernelNature.Gaussian => (0, Kernels.obtainModeThroughKernel[V, D] _)
      case KernelNature.Flat => (0, Kernels.obtainModeThroughKernel[V, D] _)
      case KernelNature.Sigmoid => (0, Kernels.obtainModeThroughKernel[V, D] _)
      case KernelNature.EuclideanKNN => (1, Kernels.euclideanKnnKernel[V] _)
      case KernelNature.KNN =>  (1, Kernels.knnKernel _)
    }

    val castedKernel = kernel.asInstanceOf[Function3[V[Double], GenSeq[V[Double]], KernelArgs, V[Double]]]

    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, V[Double]], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.v2.get
        val updatedMode = if( haveConverged ) mode else if( kernelNature == 0 ) castedKernel(mode, kernelLocality, kernelArgs) else castedKernel(mode, kernelLocalitySeq, kernelArgs)
        val modeShift = metric.d(updatedMode, mode)
        val hasConverged = if( modeShift <= epsilon ) {
          cptConvergedPoints += 1
          true
        }
        else false

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
    D[V[Double] <: Seq[Double]] <: ContinuousDistance[V[Double]]
  ](
    data: GenSeq[Cz[ID, O, V[Double]]],
    metric: D[V],
    epsilon: Double,
    maxIterations: Int,
    kernelArgs: KernelArgs
    ) = (new GradientAscent(data, epsilon, maxIterations, metric, kernelArgs)).run()
}