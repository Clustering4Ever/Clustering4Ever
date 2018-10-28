package clustering4ever.spark.clustering.mtm

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.DenseVector
import clustering4ever.spark.clustering.mtm.global.AbstractModel
import clustering4ever.spark.clustering.mtm.global.AbstractPrototype
import clustering4ever.spark.clustering.mtm.global.AbstractTrainer
import clustering4ever.spark.clustering.mtm.utils.NamedVector
import clustering4ever.spark.clustering.mtm.utils.DataGenerator
import clustering4ever.spark.clustering.mtm.utils.Output
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.ContinuousDistance
/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
object RunSom
{

  def run(
    sparkMaster: String,
    intputFile: RDD[Seq[Double]],
    outputDir: String,
    metric: ContinuousDistance[Seq] = new Euclidean[Seq](true),
    execName: String = "RunMTM",
    nbRow: Int = 10, 
    nbCol: Int = 10, 
    tmin: Double = 0.9, 
    tmax: Double = 8,
    convergeDist: Double = -0.001,
    maxIter: Int = 50,
    sep : String = ";",
    initMap: Int = 0,
    initMapFile : String = "",
    nbRealVars : Int = 10
    ) = {
    exec(
      intputFile,
      outputDir,
      metric,
      nbRow,
      nbCol,
      tmin,
      tmax,
      convergeDist,
      maxIter,
      sep,
      initMap,
      initMapFile,
      nbRealVars,
      true
    )
  }

  def exec(
    intputFile: RDD[Seq[Double]],
    outputDir: String,
    metric: ContinuousDistance[Seq] = new Euclidean[Seq](true),
    nbRow: Int = 10,
    nbCol: Int = 10,
    tmin: Double = 0.9,
    tmax: Double = 8,
    convergeDist: Double = -0.001,
		maxIter: Int = 50,
		sep : String = ";",
		initMap: Int = 0,
		initMapFile : String = "",
		nbRealVars : Int = 10,
    stop: Boolean = false
  ) =
  {
    val somOptions = Map(
      "clustering.som.nbrow" -> nbRow.toString, 
      "clustering.som.nbcol" -> nbCol.toString,
      "clustering.som.tmin" -> tmin.toString,
      "clustering.som.tmax" -> tmax.toString,
      "clustering.som.initMap" -> initMap.toString,
      "clustering.som.initMapFile" -> initMapFile,   
      "clustering.som.separator" -> sep,
      "clustering.som.nbRealVars" -> nbRealVars.toString
    )

    val trainingDataset = intputFile 

    println(s"nbRow: ${trainingDataset.count()}")
    
    val som = new SomTrainerA(metric)
    val startLearningTime = System.currentTimeMillis
    val model = som.training(trainingDataset, Some(somOptions), maxIter, convergeDist)
    val somDuration = (System.currentTimeMillis - startLearningTime) / 1000D
    
    val time = Output.write(outputDir, trainingDataset, model, nbRow, nbCol)
    (model, time)
	}
}
