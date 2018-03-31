package clustering4ever.spark.clustering.mtm.global

import org.apache.spark.mllib.linalg.DenseVector
import java.util.concurrent.TimeUnit._
import org.apache.spark.rdd.RDD
import scala.concurrent.duration.{FiniteDuration, Duration}


/**
 * Created with IntelliJ IDEA.
 * User: tug
 * Date: 14/06/13
 * Time: 12:31
 * To change this template use File | Settings | File Templates.
 */


trait AbstractTrainer extends Serializable {
  private var _it = 0
  def getLastIt = _it

  private var _converge = 1.0
  def getLastConvergence = _converge

  private var _trainingDuration = Duration.Zero
  def getLastTrainingDuration = _trainingDuration

  protected def initModel(dataset: RDD[DenseVector], modelOptions: Option[Map[String, String]])

  protected def trainingIteration(dataset: RDD[DenseVector], currentIteration: Int, maxIteration: Int): Double

  protected def getModel: AbstractModel

  final def training(dataset: RDD[DenseVector],
                     modelOptions: Option[Map[String, String]] = None,
                     maxIteration: Int = 100,
                     endConvergeDistance: Double = 0.001): AbstractModel = {

    val datasetSize = dataset.count()

    val startLearningTime = System.currentTimeMillis()

    val model = initModel(dataset, modelOptions)
    _it = 0
    _converge = 1.0

    while (_converge > endConvergeDistance && _it < maxIteration) {

      // Training iteration
      val sumConvergence = trainingIteration(dataset, _it, maxIteration)

      // process convergence
      _converge = sumConvergence / datasetSize
      _it += 1
    }

    _trainingDuration = Duration.create(System.currentTimeMillis() - startLearningTime, MILLISECONDS)
println("le model apres training est : "+getModel)

    // return the model
    getModel
  }
}
