package clustering4ever.spark.clustering.BatchStream

import breeze.linalg.{Vector, squaredDistance}
    /**
	  * Copyright: please refer to the README.md file
	  * User: ghesmoune
	  * Date: 01/01/2016
	  * Project : Square Predict (http://square-predict.net/)
	  * */
	  
class PointObj(
    val pointPartNum: Vector[Double],//the numeric part of the data-point
    val label: Int, //the real (provided) label (it is not used in the learning but for visualization and measuring performance criteria)
    val id: Int     //the identifier(=the line number) of the data-point
    ) extends Serializable {
  override def toString: String = pointPartNum.toArray.deep.mkString(", ")
}


class Prototype(
    var protoPartNum: Vector[Double],
    var idsDataAssigned : Set[Int],
    val id: Int 
    ) extends Serializable {
  override def toString: String = "node: "+id +" -> " + protoPartNum.toArray.deep.mkString(", ")
  
  def toStringIds(): String = "node: " + id +" ("+idsDataAssigned.size + " data-points)" + " -> "  + idsDataAssigned.toArray.deep.mkString(", ")

  def toStringProto(): String = protoPartNum.toArray.deep.mkString(", ") 

  def toStringCard(): String = idsDataAssigned.size.toString() 

  def toStringAss(): String = idsDataAssigned.toArray.deep.mkString(", ")
  
  def toStringId(): String = id.toString()
}
