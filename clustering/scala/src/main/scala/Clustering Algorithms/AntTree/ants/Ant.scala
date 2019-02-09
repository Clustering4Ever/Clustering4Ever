package ants

/** The ant represents the data in AntTree.
  *
  * @constructor create a new ant with features and cluster.
  * @param features the ant features's.
  * @param cluster the ant cluster's.
  */
case class Ant(val features: Vector[Double], val cluster: Any) {
  var first_time = true
}

/** The support is the starting point of the tree. */
class Support extends Ant(Vector(0), None)