package clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.util.Random
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.clustering.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumVectors
import clustering4ever.scala.kernels.{Kernels, KernelArgs}
import clustering4ever.scala.kernels.KernelNature._
import clustering4ever.scala.kernels.KernelNature
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import scala.language.higherKinds
/**
 * Mean Shift gradient ascent
 * @param kernelType defines the nature of kernel usud in the gradient ascent
 */
class GradientAscent[
  ID: Numeric,
  O,
  V <: Seq[Double],
  Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
  D <: ContinuousDistance[V]
](
  epsilon: Double,
  maxIterations: Int,
  metric: D,
  kernelArgs: KernelArgs[V, D]
) extends DataSetsTypes[ID] {

  def gradientAscent(readyToGA: GenSeq[Cz[ID, O, V]]) = {

    val haveNotConverged = false
    val kernelLocality = readyToGA.map(_.vector).seq
    var gradientAscentData: GenSeq[(Cz[ID, O, V], Boolean)] = readyToGA.map( obj => (obj.setV2(obj.vector), haveNotConverged) )
    lazy val kernelLocalitySeq: Seq[V] = kernelLocality.seq

    def kernelGradientAscent(toExplore: GenSeq[(Cz[ID, O, V], Boolean)]) = {

      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.v2.get
        val newMode = if( haveConverged ) mode
          else {
            kernelArgs.kernelType match {
              case KernelNature.Gaussian => Kernels.obtainModeThroughKernel[V, D](mode, kernelLocality, kernelArgs)
              case KernelNature.Flat => Kernels.obtainModeThroughKernel[V, D](mode, kernelLocality, kernelArgs)
              case KernelNature.Sigmoid => Kernels.obtainModeThroughKernel(mode, kernelLocality, kernelArgs)
              case KernelNature.EuclideanKNN => Kernels.euclideanKnnKernel[V](mode, kernelLocalitySeq, kernelArgs, metric.asInstanceOf[Euclidean[V]])
              case KernelNature.KNN =>  Kernels.knnKernel(mode, kernelLocalitySeq, kernelArgs)
            }
          }
        
        val modeShift = metric.d(newMode, mode)
        val hasConverged = if( modeShift <= epsilon ) {
          cptConvergedPoints += 1
          true
        }
        else false

        (obj.setV2(newMode.asInstanceOf[V]), hasConverged)
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

object GradientAscent extends DataSetsTypes[Int] {
  /**
   * @param data : an RDD[(String, Seq)] where String is the ID and Seq the rest of data
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
    kernelArgs: KernelArgs[V, D]
    ) = (new GradientAscent[ID, O, V, Cz, D](epsilon, maxIterations, metric, kernelArgs)).gradientAscent(data)
}