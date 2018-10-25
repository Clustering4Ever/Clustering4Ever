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
  V <: Seq[Double],
  Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
  D <: ContinuousDistance[V]
](
  data: GenSeq[Cz[ID, O, V]],
  epsilon: Double,
  maxIterations: Int,
  metric: D = new Euclidean[V](false),
  kernelArgs: Either[KernelArgs[V, D], KernelArgs[V, Euclidean[V]]] = Right(new KernelArgs[V, Euclidean[V]](kernelType = EuclideanKNN, k = Some(50)))
) {

  def run() = {

    val haveNotConverged = false
    val kernelLocality = data.map(_.vector)
    var gradientAscentData: GenSeq[(Cz[ID, O, V], Boolean)] = data.map( obj => (obj.setV2(obj.vector), haveNotConverged) )
    lazy val kernelLocalitySeq: Seq[V] = kernelLocality.seq
    
    val isLeft = kernelArgs.isLeft

    val kernel: Either[(V, GenSeq[V], KernelArgs[V, D]) => V, (V, Seq[V], KernelArgs[V, Euclidean[V]]) => V] = if( isLeft ) kernelArgs.left.get.kernelType match {
      case KernelNature.Gaussian => Left(Kernels.obtainModeThroughKernel[V, D])
      case KernelNature.Flat => Left(Kernels.obtainModeThroughKernel[V, D])
      case KernelNature.Sigmoid => Left(Kernels.obtainModeThroughKernel)
    }
    else {
      kernelArgs.right.get.kernelType match {
        case KernelNature.EuclideanKNN => Right(Kernels.euclideanKnnKernel[V])
        case KernelNature.KNN =>  Right(Kernels.knnKernel)
      }
    }


    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, V], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.v2.get
        val newMode = if( haveConverged ) mode else if( isLeft ) kernel.left.get(mode, kernelLocality, kernelArgs.left.get) else kernel.right.get(mode, kernelLocalitySeq, kernelArgs.right.get)
        val modeShift = metric.d(newMode, mode)
        val hasConverged = if( modeShift <= epsilon ) {
          cptConvergedPoints += 1
          true
        }
        else false

        (obj.setV2(newMode), hasConverged)
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
    Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
    D <: ContinuousDistance[V]
  ](
    data: GenSeq[Cz[ID, O, V]],
    metric: D,
    epsilon: Double,
    maxIterations: Int,
    kernelArgs: Either[KernelArgs[V, D], KernelArgs[V, Euclidean[V]]]
    ) = (new GradientAscent[ID, O, V, Cz, D](data, epsilon, maxIterations, metric, kernelArgs)).run()
}