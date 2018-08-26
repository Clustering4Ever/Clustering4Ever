package clustering4ever.scala.clustering.kprotoypes

import scala.collection.mutable
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.clustering.CommonPredictClusteringModel

/**
 * @author Beck Gaël
 **/
class KPrototypesModel[Vb <: Seq[Int], Vs <: Seq[Double]](val centers: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]], val metric: MixtDistance[Vb, Vs]) extends CommonPredictClusteringModel[BinaryScalarVector[Vb, Vs], MixtDistance[Vb, Vs]]