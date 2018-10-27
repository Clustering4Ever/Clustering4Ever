package clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import clustering4ever.clustering.ClusteringModel
import scala.collection.mutable
import clustering4ever.math.distances.Distance
import clustering4ever.clustering.CentersBasedModel
/**
 *
 */
class RLAModel[O](val centers: mutable.HashMap[Int, O], val metric: Distance[O]) extends CentersBasedModel[O, Distance[O]]