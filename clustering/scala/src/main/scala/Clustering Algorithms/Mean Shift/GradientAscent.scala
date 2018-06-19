package clustering4ever.scala.clustering.meanshift

import _root_.scala.util.Random
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.util.SumArrays
import _root_.scala.math.{min, max}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.scala.kernels.Kernels
import _root_.clustering4ever.scala.kernels.KernelNature._
import _root_.clustering4ever.scala.kernels.KernelNature
/**
 * @author Beck Gaël
 * Mean Shift gradient ascent
 * @param kernelType defines the nature of kernel usud in the gradient ascent
 */
class GradientAscent(
  var epsilon: Double,
  var maxIterations: Int,
  metric: ContinuousDistances,
  kernelType: KernelType,
  kernelArgs: immutable.Vector[String]
) extends DataSetsTypes[Int, immutable.Vector[Double]]
{
  def gradientAscent(readyToGA: Seq[(Int, Vector)], maxIterations: Int) =
  {
    var ind = 0
    val haveNotConverged = false
    var everyPointsHaveConverged = false
    var gradientAscentData: ParArray[(ID, Vector, Vector, Boolean)] = readyToGA.toParArray.map{ case (id, vector) => (id, vector, vector, haveNotConverged) }
    val kernelLocality = readyToGA.map{ case (_, vector) => vector }
    
    while( ind < maxIterations && ! everyPointsHaveConverged )
    {
      var cptConvergedPoints = 0
      
      def kernelGradientAscent(toExplore: ParArray[(ID, Vector, Vector, Boolean)]) =
      {
        val convergingData = toExplore.map{ case (id, vector, mode, haveConverged) =>
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
              case KernelNature.KNN =>  Kernels.knnKernel(mode, kernelLocality, kernelArgs.head.toInt, metric)
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

          (id, vector, newMode, hasConverged)
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

object GradientAscent extends DataSetsTypes[Int, immutable.Vector[Double]]
{
  /**
   * @param data : an RDD[(String,immutable.Vector)] where String is the ID and immutable.Vector the rest of data
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   **/
   def run(
    data: Seq[(ID, Vector)],
    metric: ContinuousDistances,
    epsilon: Double,
    maxIterations: Int,
    kernelType: KernelType,
    kernelArgs: immutable.Vector[String]
    ) =
  {
    val meanShift = new GradientAscent(epsilon, maxIterations, metric, kernelType, kernelArgs)
    meanShift.gradientAscent(data, maxIterations)
  }
}