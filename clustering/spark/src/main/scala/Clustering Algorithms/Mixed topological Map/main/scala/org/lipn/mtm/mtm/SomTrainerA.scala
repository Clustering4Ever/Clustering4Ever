
package clustering4ever.spark.clustering.mtm
import scala.math.{abs, exp}
import java.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext._
import clustering4ever.spark.clustering.mtm.global.{AbstractPrototype, AbstractModel, AbstractTrainer}
import clustering4ever.spark.clustering.mtm.utils.NamedVector
import scala.concurrent.duration.{FiniteDuration, Duration}
import org.apache.spark.mllib.linalg.DenseVector

/**
 * User: tug
 * Date: 14/06/13
 * Time: 12:35
 */
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
  var sep = DEFAULT_SEPARATOR
  var sizeRealVars: Int = SIZE_REAL_VARS
 

  protected var _somModel: SomModel = null
  protected def getModel: AbstractModel = _somModel

  protected def initModel(dataset: RDD[DenseVector], modelOptions: Option[Map[String, String]]) {
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
    // todo : replace random = 42
    var selectedDatas: Array[DenseVector] = Array()
    if (initMap == 0) {    
       selectedDatas = {
      dataset.takeSample(withReplacement = false, mapSize, new Random().nextInt())
    }
    } else {
       selectedDatas = {
//        scala.io.Source.fromFile(initMapFile).getLines().toArray.map(x => new DenseVector(x.split(sep).map(_.toDouble)))
        scala.io.Source.fromFile(initMapFile).getLines().drop(1).toArray.map(x => new DenseVector(x.split(sep).map(_.toDouble)))

       }
    }

    // todo : Check /nbCol et %nbCOl
    val neuronMatrix = Array.tabulate(mapSize)(id => new SomNeuron(id, id/nbCol, id%nbCol, selectedDatas(id)))
    _somModel = new SomModel(nbRow, nbCol, neuronMatrix)
  }//init model

  protected def trainingIteration(dataset: RDD[DenseVector], currentIteration: Int, maxIteration: Int): Double = {
    
    val t = processT(maxIteration, currentIteration)

    // create som observations
    val mapping = dataset.map{d =>
      val bestNeuron = _somModel.findClosestPrototype(d).asInstanceOf[SomNeuron]
      
      //ML: à rentrer dans la condition
      var mapBin: scala.collection.immutable.Vector[(Int, Int)] = scala.collection.immutable.Vector()
      
      //binary part
      if (d.size > this.sizeRealVars){
        val d2: scala.collection.immutable.Vector[Double] = d.toArray.drop(sizeRealVars).toVector.asInstanceOf[scala.collection.immutable.Vector[Double]]
        mapBin = d2.map(x => if (x == 1) (1,0) else (0,1))
      }


      _somModel.prototypes.map{proto =>
        val neuron = proto.asInstanceOf[SomNeuron]
        val factor = neuron.factorDist(bestNeuron, t) // K(delta(.-.)/T)
             
        //binary part
        var mapBinPondere: scala.collection.immutable.Vector[(Double, Double)] = scala.collection.immutable.Vector()
       
        //ML:ajouter la condition (d.length > this.sizeRealVars), sinon vecteur vide
        if (mapBin.size > 0) {
          mapBinPondere = mapBin.map(x => (x._1 * factor, x._2 * factor))
        }
        
        //ML: dans le cas de non présence de réelle vecteur vide, pareil pour les varibales binaires
        new SomObsA(new DenseVector(d.toArray.take(sizeRealVars).map(_ * factor)), factor, mapBinPondere, neuron.id)
        // ligne originale
        //new SomObsA(Vector(d.toArray.take(sizeRealVars)) * factor, factor, mapBinPondere, neuron.id)

      }
    } //end mapping

    // Concat observations
    val concatObs = mapping.reduce{(obs1, obs2) =>
      for (i <- 0 until obs1.length) {
        obs1(i) += obs2(i)
      }
      obs1
    }

    // Update model and process convergence distance
    //val x: Array[Double] = concatObs.map(_somModel.update)
    concatObs.map(_somModel.update).sum
    
  }//end trainingIteration

  //protected def processT(maxIt:Int, currentIt:Int) = maxIt.toFloat - currentIt
   protected def processT(maxIt:Int, currentIt:Int) =
   {
      this.tmax*math.pow(this.tmin/this.tmax,currentIt/(maxIt.toFloat-1))
   }

  protected class SomModel(val nbRow: Int, val nbCol: Int, neurons: Array[SomNeuron]) extends AbstractModel(neurons.asInstanceOf[Array[AbstractPrototype]]) {

    // Update the data point of the neuron
    // and return the distance between the new and the old point
    def update(obs: SomObsA) = neurons(obs.neuronId).update(obs.compute)


    override def toString: String = {
      var str = ""
      for(neuron <- neurons) {
        str += neuron+"\n"
      }
      str
    }
  }

  protected class SomNeuron(id: Int, val row: Int, val col: Int, point: DenseVector) extends AbstractPrototype(id, point) {
    def factorDist(neuron: SomNeuron, T: Double): Double = {
      exp(-(abs(neuron.row - row) + abs(neuron.col - col)) / T)
    }

    override def toString: String = {
      "("+row+", "+col+") -> "+point
    }
  }

  protected class SomObsA(var numerator:DenseVector, var denominator: Double, var mapBinPonderation: scala.collection.immutable.Vector[(Double, Double)], val neuronId: Int) extends Serializable
  {
    def +(obs: SomObsA): SomObsA =
    {
      //ML:que lorsqu'on a des données réelles
      numerator = new DenseVector( obs.numerator.toArray.zip(numerator.toArray).map( x => x._1 + x._2 ) )
      denominator += obs.denominator
      

      // calcul de la somme des pondÃ©ration des 1 et des 0
     //ML:ajouter la condition (d.length > this.sizeRealVars)
      
      var mapBinPonderation2: scala.collection.immutable.Vector[(Double, Double)] = scala.collection.immutable.Vector()
    if ( mapBinPonderation.size > 0 )
      {
      for (i <-0 until mapBinPonderation.size){
        val c1: Double = mapBinPonderation(i)._1 + obs.mapBinPonderation(i)._1
        val c0: Double = mapBinPonderation(i)._2 + obs.mapBinPonderation(i)._2
         mapBinPonderation2==mapBinPonderation2 :+ (c1, c0)
      }
      mapBinPonderation = mapBinPonderation2
    }
      
      this
    }

    //def compute = numerator / denominator
    def compute = {
      // Linge originale
      //val newPointsReal = numerator / denominator
      val newPointsReal = new DenseVector( numerator.toArray.map(_ / denominator) )
      
      // calcul de la mediane
      //ML:ajouter la condition (d.length > this.sizeRealVars)
      //var newPointsBin:Array[Double]=Array()
      
      var newPointsBin: scala.collection.immutable.Vector[Double] = scala.collection.immutable.Vector()
      
      if ( mapBinPonderation.size > 0 )
      {
        newPointsBin = mapBinPonderation.map {e =>
        if (e._1 >= e._2) 1.0 else 0.0}
      }
     
      // concatenation de la partie real et binaire
      new DenseVector(newPointsReal.toArray ++ newPointsBin) 
       
    }

    override def toString = numerator.toString()+" : "+denominator.toString
  }//end SomObsA



  def purity(dataset: RDD[NamedVector]): Double = {
    //val nbRealClass = dataset.map(_.cls).reduce(case(cls1,cls2))

    val sumAffectedDatas = dataset.map(d => ((_somModel.findClosestPrototype(d).id, d.cls), 1))
      .reduceByKey{case (sum1, sum2) => sum1+sum2}

    val maxByCluster = sumAffectedDatas.map(sa => (sa._1._1, sa._2))
      .reduceByKey{case (sum1, sum2) => sum1.max(sum2) }
      .map(_._2)
      .collect()

    maxByCluster.sum / dataset.count().toDouble
  }

  def affectations(dataset: RDD[NamedVector]): RDD[(Int, Int)] = {
    dataset.map(d => (d.cls, _somModel.findClosestPrototype(d).id))
  }
} //end SomTrainerA

 class pointObj(
    val data: DenseVector,//the numeric part of the data-point
    //val label: Int,            //the real (provided) label
    val id: Int               //the identifier(=numeroLigne) of the data-point
    ) extends Serializable {
  override def toString: String = {" "
    //data.toArray.deep.mkString(", ") + pointPartBin.toArray.deep.mkString(", ")
    /*"partieNumerique -> "+pointPartNum.toArray.deep.mkString("[", ", ", "]") +
    "; partieBinaire -> "+pointPartBin.toArray.deep.mkString("[", ", ", "]")*/ 
  } 
 }