package org.clustering4ever.clustering.optics

import scala.language.higherKinds
import org.clustering4ever.clustering.{ClusteringModelLocal, ClusteringModelLocalBinary, ClusteringModelLocalMixed, ClusteringModelLocalScalar}
import org.clustering4ever.clustering.models.MetricModel
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.vectors.{BinaryVector, GVector, MixedVector, ScalarVector}

import scala.collection.{GenSeq, immutable}

trait OpticsAlgoModelAncestor[V <: GVector[V], D <: Distance[V]] {

  /** Get clusters from order
    *
    * @param order Uncorvered points
    * @param acc Covered points
    * @param cluster Current cluster ID
    * @return Map with id as key and cluster as value
    */
  @annotation.tailrec
  final protected def getClusters(order:  List[(Long, Option[Double])], acc: immutable.LongMap[Int], cluster: Int): immutable.LongMap[Int] = {
    order match {
      case (id, rd):: t =>
        val nextCluster = if(rd.isDefined) cluster else cluster + 1
        getClusters(t, acc + (id -> nextCluster), nextCluster)
      case Nil => acc
    }
  }
}

trait OpticsModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with MetricModel[V, D] with OpticsAlgoModelAncestor[V, D] {

  def order: List[(Long, Option[Double])]

  val clusters: immutable.LongMap[Int] = getClusters(order, immutable.LongMap.empty[Int], -1)

  protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    data.map(cz => cz.addClusterIDs(clusters(cz.id))).asInstanceOf[GS[Cz[O, V]]]
  }

  /**
    * This method work only with input dataset which generate this model, please use others method for new set of points
    *
    * @return the clusterized dataset
    */
  final def obtainInputDataClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    obtainClustering(data)
  }

}


final case class OpticsModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](metric: D[V], order: List[(Long, Option[Double])]) extends OpticsModelAncestor[V, D[V]] {
  val algorithmID = org.clustering4ever.extensibleAlgorithmNature.Optics
}

final case class OpticsModelScalar[D <: ContinuousDistance](metric: D, order: List[(Long, Option[Double])]) extends OpticsModelAncestor[ScalarVector, D] with ClusteringModelLocalScalar {
  val algorithmID = org.clustering4ever.extensibleAlgorithmNature.OpticsScalar
}

final case class OpticsModelBinary[D <: BinaryDistance](metric: D, order: List[(Long, Option[Double])]) extends OpticsModelAncestor[BinaryVector, D] with ClusteringModelLocalBinary {
  val algorithmID = org.clustering4ever.extensibleAlgorithmNature.OpticsBinary
}

final case class OpticsModelMixed[D <: MixedDistance](metric: D, order: List[(Long, Option[Double])]) extends OpticsModelAncestor[MixedVector, D] with ClusteringModelLocalMixed {
  val algorithmID = org.clustering4ever.extensibleAlgorithmNature.OpticsMixed
}
