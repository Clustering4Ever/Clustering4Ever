package clustering4ever.spark.clustering.mtm.global

//import org.apache.spark.util.Vector

import org.apache.spark.mllib.linalg.{Vectors, Vector, DenseVector}

//import org.apache.spark.util

/**
 * Created with IntelliJ IDEA.
 * User: tug
 * Date: 14/06/13
 * Time: 12:42
 * To change this template use File | Settings | File Templates.
 */
abstract class AbstractPrototype(val id: Int, var _point: DenseVector) extends Serializable {
  def update(newPoint: DenseVector): Double = {
 //   val dist = _point.dist(newPoint)
  val dist = Vectors.sqdist(_point, newPoint)

    _point = newPoint
    dist
  }

 // def dist(data: Vector) = _point.dist(data) // a modifier: - ajouter une pondération fixe; - ajouter une pondération adaptative
def dist(data: DenseVector) = Vectors.sqdist(_point,data) // a modifier: - ajouter une pondération fixe; - ajouter une pondération adaptative

//def dist(prototype: AbstractPrototype) = _point.dist(prototype._point)
def dist(prototype: AbstractPrototype) = Vectors.sqdist(_point,prototype._point)
  
}
