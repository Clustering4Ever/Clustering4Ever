package clustering4ever.scala.clustering.meanshift

import scala.util.Random
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.clustering.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumVectors
import clustering4ever.scala.kernels.Kernels
import clustering4ever.scala.kernels.KernelNature._
import clustering4ever.scala.kernels.KernelNature
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable

/**
 * @author Beck Gaël
 * Mean Shift gradient ascent
 * @param kernelType defines the nature of kernel usud in the gradient ascent
 */
class GradientAscent[ID: Numeric, Obj, V <: Seq[Double], Cz <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](
  epsilon: Double,
  maxIterations: Int,
  metric: D,
  kernelType: KernelType,
  kernelArgs: immutable.Vector[String]
) extends DataSetsTypes[ID]
{
  def gradientAscent(readyToGA: GenSeq[Cz]) =
  {
    val haveNotConverged = false
    val kernelLocality = readyToGA.map(_.vector).seq

    var gradientAscentData: GenSeq[(Cz, Boolean)] = readyToGA.map( obj => (obj.setV2(obj.vector), haveNotConverged) )

    lazy val kernelLocalitySeq: Option[Seq[V]] = kernelType match {
      case KernelNature.EuclideanKNN => Some(kernelLocality)
      case KernelNature.KNN => Some(kernelLocality)
      case _ => None
    }

    def kernelGradientAscent(toExplore: GenSeq[(Cz, Boolean)]) =
    {
      var cptConvergedPoints = 0
      
      val convergingData = toExplore.map{ case (obj, haveConverged) =>
        val mode = obj.v2
        val newMode = if( haveConverged ) mode
          else {
            kernelType match
            {
              case kernel if( kernel == KernelNature.Gaussian || kernel == KernelNature.Flat ) =>  Kernels.obtainModeThroughKernel[V](mode, kernelLocality, kernelArgs.head.toDouble, kernelType, metric)
              case KernelNature.EuclideanKNN =>  Kernels.euclideanKnnKernel[V](mode, kernelLocalitySeq.get, kernelArgs.head.toInt, metric.asInstanceOf[Euclidean[V]])
              case KernelNature.KNN =>  Kernels.knnKernel(mode, kernelLocalitySeq.get, kernelArgs.head.toInt, metric)
              case KernelNature.Sigmoid =>  Kernels.obtainModeThroughSigmoid(mode, kernelLocality, kernelArgs.head.toDouble, kernelArgs(1).toDouble)
              case _ =>  Kernels.knnKernel(mode, kernelLocalitySeq.get, 40, metric)
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

object GradientAscent extends DataSetsTypes[Int]
{
  /**
   * @param data : an RDD[(String, Seq)] where String is the ID and Seq the rest of data
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   **/
   def run[ID: Numeric, Obj, V <: Seq[Double], Cz <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](
    data: GenSeq[Cz],
    metric: D,
    epsilon: Double,
    maxIterations: Int,
    kernelType: KernelType,
    kernelArgs: immutable.Vector[String]
    ) =
  {
    val meanShift = new GradientAscent[ID, Obj, V, Cz, D](epsilon, maxIterations, metric, kernelType, kernelArgs)
    meanShift.gradientAscent(data)
  }
}