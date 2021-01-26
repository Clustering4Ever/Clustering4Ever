package org.clustering4ever.clustering.selforganizedmap.global

import org.apache.spark.rdd.RDD

/**
 * @author Sarazin Tugdual
 * @author Beck Gaël
 */
class PointObj(val data: Array[Double], val id: Int) extends Serializable
{
  override def toString: String = " " 
}
 

abstract class AbstractModel(val prototypes: Array[AbstractPrototype]) extends Serializable
{
  def size() = prototypes.length

  def findClosestPrototype(data: Array[Double]): AbstractPrototype = prototypes.minBy( proto => proto.dist(data) )
  
  def findClosestPrototypeId(data: Array[Double]): AbstractPrototype = prototypes.minBy( proto => proto.dist(data) )

  def apply(i: Int) = prototypes(i)

  def assign(dataset: RDD[PointObj]): RDD[(Int, Int)] = dataset.map( d => (this.findClosestPrototype(d.data).id, d.id) )
}
