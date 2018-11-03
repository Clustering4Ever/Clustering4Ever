package clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import clustering4ever.clustering.ClusteringModel
import scala.collection.mutable
import clustering4ever.math.distances.Distance
import clustering4ever.clustering.CenterOrientedModel
/**
 *
 */
class RLAModel[O, D <: Distance[O]](val centers: mutable.HashMap[Int, O], val metric: D) extends CenterOrientedModel[O, D]