package clustering4ever.spark.clustering.mtm

import scala.math.{abs, exp, max}
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext._
import clustering4ever.spark.clustering.mtm.global.{AbstractPrototype, AbstractModel, AbstractTrainer}
import clustering4ever.spark.clustering.mtm.utils.NamedVector
import scala.concurrent.duration.{FiniteDuration, Duration}
import org.apache.spark.mllib.linalg.DenseVector

/**
 * @author Sarazin Tugdual & Lebbah Mustapha & Beck Gaël
 **/
class SomTrainerA extends AbstractTrainer
{
  val DEFAULT_SOM_ROW = 10
  val DEFAULT_SOM_COL = 10
  val DEFAULT_TMAX = 8
  val DEFAULT_TMIN = 1
  val DEFAULT_INITMAP = 0
  val DEFAULT_INITMAPFile = ""
  val DEFAULT_SEPARATOR = ""
  val SIZE_REAL_VARS = 10
  
  var tmax: Double = DEFAULT_TMAX
  var tmin: Double = DEFAULT_TMIN
  var initMap: Int = DEFAULT_INITMAP
  var initMapFile: String = DEFAULT_INITMAPFile
  var sep: String = DEFAULT_SEPARATOR
  var sizeRealVars: Int = SIZE_REAL_VARS
 

  protected var _somModel: Option[SomModel] = None
  protected def getModel: AbstractModel = _somModel.get

  protected def initModel(dataset: RDD[DenseVector], modelOptions: Option[Map[String, String]]) =
  {
    var nbRow = DEFAULT_SOM_ROW
    var nbCol = DEFAULT_SOM_COL
    if ( modelOptions.isDefined )
    {
      nbRow = modelOptions.get("clustering.som.nbrow").toInt
      nbCol = modelOptions.get("clustering.som.nbcol").toInt
      tmax = modelOptions.get.get("clustering.som.tmax").map(_.toDouble).getOrElse(DEFAULT_TMAX)
      tmin = modelOptions.get.get("clustering.som.tmin").map(_.toDouble).getOrElse(DEFAULT_TMIN)
      initMap = modelOptions.get.get("clustering.som.initMap").map(_.toInt).getOrElse(DEFAULT_INITMAP)
      initMapFile = modelOptions.get.get("clustering.som.initMapFile").map(_.toString).getOrElse(DEFAULT_INITMAPFile)
      sep = modelOptions.get.get("clustering.som.separator").map(_.toString).getOrElse(DEFAULT_SEPARATOR)
      sizeRealVars = modelOptions.get.get("clustering.som.nbRealVars").map(_.toInt).getOrElse(SIZE_REAL_VARS)
    }


    val mapSize = nbRow * nbCol
    val selectedDatas: Array[DenseVector] = if( initMap == 0 )
    {    
      dataset.takeSample(withReplacement = false, mapSize, Random.nextInt)
    }
    else
    {
      scala.io.Source.fromFile(initMapFile).getLines().drop(1).toArray.map( line => new DenseVector(line.split(sep).map(_.toDouble)))
    }

    // todo : Check /nbCol et %nbCOl
    val neuronMatrix = Array.tabulate(mapSize)(id => new SomNeuron(id, id/nbCol, id%nbCol, selectedDatas(id)))
    _somModel = Some(new SomModel(nbRow, nbCol, neuronMatrix))
  }
  //init model

  protected def trainingIteration(dataset: RDD[DenseVector], currentIteration: Int, maxIteration: Int): Double =
  {
    
    val t = processT(maxIteration, currentIteration)

    // create som observations
    val mapping = dataset.map( d =>
    {
      val bestNeuron = _somModel.get.findClosestPrototype(d).asInstanceOf[SomNeuron]
      //ML: à rentrer dans la condition
      val mapBin: scala.collection.immutable.Vector[(Int, Int)] = if (d.size > this.sizeRealVars)
      {
        d.toArray.drop(sizeRealVars).toVector.asInstanceOf[scala.collection.immutable.Vector[Double]].map( x => if (x == 1) (1, 0) else (0, 1) )
      }
      else
      {
        scala.collection.immutable.Vector.empty[(Int, Int)]
      }

      _somModel.get.prototypes.map{proto =>
      val neuron = proto.asInstanceOf[SomNeuron]
      val factor = neuron.factorDist(bestNeuron, t) // K(delta(.-.)/T)
         
      //binary part
      val mapBinPondere: scala.collection.immutable.Vector[(Double, Double)] = //ML:ajouter la condition (d.length > this.sizeRealVars), sinon vecteur vide
      if( mapBin.size > 0 )
      {
        mapBin.map{ case (x, y) => (x * factor, y * factor) }
      }
      else
      {
        scala.collection.immutable.Vector.empty[(Double, Double)]
      }

      //ML: dans le cas de non présence de réelle vecteur vide, pareil pour les varibales binaires
      new SomObsA(new DenseVector(d.toArray.take(sizeRealVars).map(_ * factor)), factor, mapBinPondere, neuron.id)
      // ligne originale
      //new SomObsA(Vector(d.toArray.take(sizeRealVars)) * factor, factor, mapBinPondere, neuron.id)
      }
    }) //end mapping

    // Concat observations
    val concatObs = mapping.reduce{ (obs1, obs2) =>
    {
      for ( i <- obs1.indices )
      {
        obs1(i) += obs2(i)
      }
      obs1
    }}

    // Update model and process convergence distance
    //val x: Array[Double] = concatObs.map(_somModel.update)
    concatObs.map(_somModel.get.update).sum
    
  }//end trainingIteration

  //protected def processT(maxIt:Int, currentIt:Int) = maxIt.toFloat - currentIt
  protected def processT(maxIt:Int, currentIt:Int) =
  {
    this.tmax*math.pow(this.tmin/this.tmax,currentIt/(maxIt.toFloat-1))
  }

  protected class SomModel(val nbRow: Int, val nbCol: Int, neurons: Array[SomNeuron]) extends AbstractModel(neurons.asInstanceOf[Array[AbstractPrototype]])
  {
    // Update the data point of the neuron
    // and return the distance between the new and the old point
    def update(obs: SomObsA) =
    {
      neurons(obs.neuronId).update(obs.compute)
    }

    override def toString(): String =
    {
      neurons.mkString("\n")
    }
  }

  protected class SomNeuron(id: Int, val row: Int, val col: Int, point: DenseVector) extends AbstractPrototype(id, point)
  {
    def factorDist(neuron: SomNeuron, t: Double): Double =
    {
      exp(-(abs(neuron.row - row) + abs(neuron.col - col)) / t)
    }

    override def toString(): String =
    {
      "(" + row + ", " + col + ") -> " + point
    }
  }

  protected class SomObsA(var numerator: DenseVector, var denominator: Double, var mapBinPonderation: scala.collection.immutable.Vector[(Double, Double)], val neuronId: Int) extends Serializable
  {
    def +(obs: SomObsA): SomObsA =
    {
      //ML:que lorsqu'on a des données réelles
      numerator = new DenseVector( obs.numerator.toArray.zip(numerator.toArray).map( x => x._1 + x._2 ) )
      denominator += obs.denominator
        

      // TO DO
      // calcul de la somme des pondÃ©ration des 1 et des 0
      //ML:ajouter la condition (d.length > this.sizeRealVars)
      val mapBinPonderation2: scala.collection.immutable.Vector[(Double, Double)] = if ( mapBinPonderation.size > 0 )
      {
        for (i <-0 until mapBinPonderation.size)
        {
          val c1: Double = mapBinPonderation(i)._1 + obs.mapBinPonderation(i)._1
          val c0: Double = mapBinPonderation(i)._2 + obs.mapBinPonderation(i)._2
          //mapBinPonderation2 == mapBinPonderation2 :+ (c1, c0)
        }
        //mapBinPonderation = mapBinPonderation2
        scala.collection.immutable.Vector.empty[(Double, Double)]
      }
      else
      {
        scala.collection.immutable.Vector.empty[(Double, Double)] 
      } 

      this
    }

    //def compute = numerator / denominator
    def compute() =
    {
      // Linge originale
      //val newPointsReal = numerator / denominator
      val newPointsReal = new DenseVector( numerator.toArray.map(_ / denominator) )
      
      // calcul de la mediane
      //ML:ajouter la condition (d.length > this.sizeRealVars)
      //var newPointsBin:Array[Double]=Array()
      val newPointsBin: scala.collection.immutable.Vector[Double] = if( mapBinPonderation.size > 0 )
      {
        mapBinPonderation.map( e => if( e._1 >= e._2 ) 1D else 0D )
      }
      else
      {
        scala.collection.immutable.Vector.empty[Double]
      }
     
      // concatenation de la partie real et binaire
      new DenseVector(newPointsReal.toArray ++ newPointsBin) 
       
    }

    override def toString =
    {
      numerator.toString() + " : " + denominator.toString
    }
  }//end SomObsA



  def purity(dataset: RDD[NamedVector]): Double =
  {
    val sumAffectedDatas = dataset.map( d => ((_somModel.get.findClosestPrototype(d).id, d.cls), 1) ).reduceByKey(_ + _)

    val maxByCluster = sumAffectedDatas.map{ case ((id, cls), count) => (id, count) }.reduceByKey(max).map(_._2).collect

    maxByCluster.sum.toDouble / dataset.count()
  }

  def affectations(dataset: RDD[NamedVector]): RDD[(Int, Int)] =
  {
    dataset.map( d => (d.cls, _somModel.get.findClosestPrototype(d).id) )
  }
} //end SomTrainerA

class pointObj(
  val data: DenseVector,//the numeric part of the data-point
  //val label: Int,            //the real (provided) label
  val id: Int               //the identifier(=numeroLigne) of the data-point
  ) extends Serializable
{
  override def toString: String =
  {
    " "
    //data.toArray.deep.mkString(", ") + pointPartBin.toArray.deep.mkString(", ")
    /*"partieNumerique -> "+pointPartNum.toArray.deep.mkString("[", ", ", "]") +
    "; partieBinaire -> "+pointPartBin.toArray.deep.mkString("[", ", ", "]")*/ 
  }
}