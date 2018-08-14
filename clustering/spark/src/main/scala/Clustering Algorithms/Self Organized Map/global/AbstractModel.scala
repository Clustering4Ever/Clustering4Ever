package clustering4ever.spark.clustering.mtm.global

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.DenseVector

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
class PointObj(
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
 

abstract class AbstractModel(val prototypes: Array[AbstractPrototype]) extends Serializable
{
  def size() = prototypes.size

  def findClosestPrototype(data: DenseVector): AbstractPrototype =
  {
    prototypes.minBy( proto => proto.dist(data) )
  }
  
  def findClosestPrototypeId(data: DenseVector): AbstractPrototype =
  {
    prototypes.minBy( proto => proto.dist(data) )
  }  

  def apply(i: Int) =
  {
    prototypes(i)
  }

  def assign(dataset: RDD[PointObj]): RDD[(Int, Int)] =
  {
    dataset.map( d => (this.findClosestPrototype(d.data).id, d.id) )
  }
}
