package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import org.clustering4ever.clustering.ClusteringModel
import scala.collection.mutable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clustering.CenterOrientedModel
/**
 *
 */
class RLAModel[O, D <: Distance[O]](val centers: mutable.HashMap[Int, O], val metric: D) extends CenterOrientedModel[O, D]