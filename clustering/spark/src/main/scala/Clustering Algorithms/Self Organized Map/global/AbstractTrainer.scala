package org.clustering4ever.spark.clustering.mtm.global

import org.apache.spark.mllib.linalg.DenseVector
import java.util.concurrent.TimeUnit._
import org.apache.spark.rdd.RDD
import scala.concurrent.duration.{FiniteDuration, Duration}

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
trait AbstractTrainer extends Serializable
{
  private var iter = 0
  def getLastIt = iter

  private var converge = 1D
  def getLastConvergence() = converge

  private var trainingDuration = Duration.Zero
  def getLastTrainingDuration = trainingDuration

  protected def initModel(dataset: RDD[Seq[Double]], modelOptions: Option[Map[String, String]])

  protected def trainingIteration(dataset: RDD[Seq[Double]], currentIteration: Int, maxIteration: Int): Double

  protected def getModel: AbstractModel

  final def training(
    dataset: RDD[Seq[Double]],
    modelOptions: Option[Map[String, String]] = None,
    maxIteration: Int = 100,
    endConvergeDistance: Double = 0.001
  ): AbstractModel =
  {
    val datasetSize = dataset.count()

    val startLearningTime = System.currentTimeMillis()

    val model = initModel(dataset, modelOptions)
    iter = 0
    converge = 1D

    while (converge > endConvergeDistance && iter < maxIteration)
    {
      // Training iteration
      val sumConvergence = trainingIteration(dataset, iter, maxIteration)
      // process convergence
      converge = sumConvergence / datasetSize
      iter += 1
    }

    trainingDuration = Duration.create(System.currentTimeMillis() - startLearningTime, MILLISECONDS)
    println("le model apres training est : "+getModel)

    // return the model
    getModel
  }
}
