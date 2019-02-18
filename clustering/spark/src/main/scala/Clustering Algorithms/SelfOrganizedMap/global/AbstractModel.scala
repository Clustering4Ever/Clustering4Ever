package org.clustering4ever.spark.clustering.mtm

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.DenseVector

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
class PointObj(val data: Seq[Double], val id: Int) extends Serializable
{
  override def toString: String = " " 
}
 

abstract class AbstractModel(val prototypes: Array[AbstractPrototype]) extends Serializable
{
  def size() = prototypes.size

  def findClosestPrototype(data: Seq[Double]): AbstractPrototype = prototypes.minBy( proto => proto.dist(data) )
  
  def findClosestPrototypeId(data: Seq[Double]): AbstractPrototype = prototypes.minBy( proto => proto.dist(data) )

  def apply(i: Int) = prototypes(i)

  def assign(dataset: RDD[PointObj]): RDD[(Int, Int)] = dataset.map( d => (this.findClosestPrototype(d.data).id, d.id) )
}
