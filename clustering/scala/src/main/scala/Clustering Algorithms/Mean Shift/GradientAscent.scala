package clustering4ever.scala.clustering.meanshift

import scala.util.Random
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumArrays
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
class GradientAscent[ID: Numeric, Obj](
  var epsilon: Double,
  var maxIterations: Int,
  metric: ContinuousDistances,
  kernelType: KernelType,
  kernelArgs: immutable.Vector[String]
) extends DataSetsTypes[ID, immutable.Seq[Double]]
{
  def gradientAscent(readyToGA: GenSeq[(ID, RealClusterizable[ID, Obj])], maxIterations: Int) =
  {
    var ind = 0
    val haveNotConverged = false
    var everyPointsHaveConverged = false
    var gradientAscentData: GenSeq[(ID, RealClusterizable[ID, Obj], immutable.Seq[Double], Boolean)] = readyToGA.par.map{ case (id, obj) => (id, obj, obj.vector, haveNotConverged) }
    val kernelLocality = readyToGA.map{ case (_, obj) => obj.vector }
    lazy val kernelLocalitySeq = kernelType match
    {
      case KernelNature.EuclideanKNN => Some(kernelLocality.seq)
      case KernelNature.KNN => Some(kernelLocality.seq)
      case _ => None
    }

    while( ind < maxIterations && ! everyPointsHaveConverged )
    {
      var cptConvergedPoints = 0
      
      def kernelGradientAscent(toExplore: GenSeq[(ID, RealClusterizable[ID, Obj], immutable.Seq[Double], Boolean)]) =
      {
        val convergingData = toExplore.map{ case (id, obj, mode, haveConverged) =>
        { 
          val newMode = if( haveConverged )
          {
            mode
          }
          else
          {
            kernelType match
            {
              case kernel if( kernel == Gaussian || kernel == Flat ) =>  Kernels.obtainModeThroughKernel(mode, kernelLocality, kernelArgs.head.toDouble, kernelType, metric)
              case KernelNature.EuclideanKNN =>  Kernels.euclideanKnnKernel(mode, kernelLocalitySeq.get, kernelArgs.head.toInt, metric.asInstanceOf[Euclidean])
              case KernelNature.KNN =>  Kernels.knnKernel(mode, kernelLocalitySeq.get, kernelArgs.head.toInt, metric)
              case KernelNature.Sigmoid =>  Kernels.obtainModeThroughSigmoid(mode, kernelLocality, kernelArgs.head.toDouble, kernelArgs(1).toDouble)
            }
          }
          
          val modeShift = metric.d(newMode, mode)
          val hasConverged = if( modeShift <= epsilon )
          {
            cptConvergedPoints += 1
            true
          }
          else 
          {
            false
          }

          (id, obj, newMode, hasConverged)
        }}

        convergingData
      }

      gradientAscentData = kernelGradientAscent(gradientAscentData)
      everyPointsHaveConverged = cptConvergedPoints == gradientAscentData.size
      ind += 1
    }
      gradientAscentData
  }
}

object GradientAscent extends DataSetsTypes[Int, immutable.Seq[Double]]
{
  /**
   * @param data : an RDD[(String,immutable.Seq)] where String is the ID and immutable.Seq the rest of data
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   **/
   def run[ID: Numeric, Obj](
    data: GenSeq[(ID, RealClusterizable[ID, Obj])],
    metric: ContinuousDistances,
    epsilon: Double,
    maxIterations: Int,
    kernelType: KernelType,
    kernelArgs: immutable.Vector[String]
    ) =
  {
    val meanShift = new GradientAscent[ID, Obj](epsilon, maxIterations, metric, kernelType, kernelArgs)
    meanShift.gradientAscent(data, maxIterations)
  }
}