package org.clustering4ever.clustering.anttree
/**
 * @author Waris Radji
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.{ClusteringModelLocal, ClusteringModelLocalBinary, ClusteringModelLocalScalar}
import org.clustering4ever.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.models.MetricModel
import org.clustering4ever.roottraits._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MutableGraph}

import scala.collection.{GenSeq, immutable, mutable}
import scala.language.higherKinds
/**
 * @tparam T une description des paramètres génériques
 * @tparam E une description des paramètres génériques
 */
final case class Tree[T, E[X] <: EdgeLikeIn[X]](val graph: MutableGraph[T, E]) {
  /**
   * The principals cluster aim to determine the clusters of the other nodes, these are the nodes that are directly connected to the support
   */
  final private val principalsCLuster = mutable.ArrayBuffer.empty[graph.NodeT]
  /** Set principals cluster from a support
    *
    * @param support the support
    */
  final def obtainPrincipalCluster(support: T): Unit = principalsCLuster ++= graph.get(support).diSuccessors

  /**
    * @return current principals cluster
    */
  final def getPrincipalCluster: mutable.ArrayBuffer[graph.NodeT] = principalsCLuster
}
/**
 * @tparam V the vector nature
 * @tparam D the distance nature
 */
trait AntTreeAlgoModelAncestor[V <: GVector[V], D <: Distance[V]] {
  /** Get all successors of a node
    *
    * @param xpos The node
    * @param tree The tree
    * @return Set of all successors
    */
  final def allSuccessors(xpos: (Long, Option[V]), tree: Tree[(Long, Option[V]), UnDiEdge]): immutable.Set[(Long, Option[V])] = {
    val node = tree.graph.get(xpos)
    node.withSubgraph().map(_.value).toSet - node
  }
  /** Get all successors directly next to the node
    *
    * @param xpos The node
    * @param tree The tree
    * @return Set of direct successors
    */
  final def directSuccessors(xpos: (Long, Option[V]), tree: Tree[(Long, Option[V]), UnDiEdge]): immutable.Set[(Long, Option[V])] = {
    tree.graph.get(xpos).diSuccessors.map(_.value)
  }
}
/**
 * @tparam V the vector nature
 * @tparam D the distance nature
 */
trait AntTreeModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with MetricModel[V, D] with AntTreeAlgoModelAncestor[V, D] {
  /**
   * A tree that has the data as its node
   */
  val tree: Tree[(Long, Option[V]), UnDiEdge]

  val metric: D // je n'ai pas mis de commentaires ici car ce champs herite de MetricModel qui à déjà une description, sinon on va l'override
  /**
   * The support of the tree that does not represent data
   */
  private final val supportID = Long.MinValue

  protected[clustering4ever] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    val supportChild = tree.getPrincipalCluster.map(e => allSuccessors(e, tree))
    data.map( cz => cz.addClusterIDs(supportChild.indexWhere(_.contains((cz.id, Some(cz.v))))) ).asInstanceOf[GS[Cz[O, V]]]
  }
  /** Prediction of a point on the tree by going back to the root
    *
    * @param point the point to predict
    * @return the cluster of the point
    */
  final def predictOnePoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](point: Cz[O, V]): Int = {
    tree.getPrincipalCluster.indexOf(tree.getPrincipalCluster.maxBy( c => metric.d(c._2.get, point.v) ))
  }
  /** Prediction of all points on the tree
    *
    * @param data data to predict
    * @return seq of dara with cluster IDs
    */
  final def predict[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    // à simplifier !
    data.map( cz => cz.addClusterIDs(predictOnePoint(cz)) ).asInstanceOf[GS[Cz[O, V]]]

  }

}
/**
 * @tparam D the distance nature
 */
final case class AntTreeModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val metric: D[V], val tree: Tree[(Long, Option[V]), UnDiEdge]) extends AntTreeModelAncestor[V, D[V]]{

  val algorithmID = org.clustering4ever.roottraits.AntTree

}

final case class AntTreeModelScalar[D <: ContinuousDistance](val metric: D, val tree: Tree[(Long, Option[ScalarVector]), UnDiEdge]) extends AntTreeModelAncestor[ScalarVector, D] with ClusteringModelLocalScalar{

  val algorithmID = org.clustering4ever.roottraits.AntTreeScalar

}

final case class AntTreeModelBinary[D <: BinaryDistance](val metric: D, val tree: Tree[(Long, Option[BinaryVector]), UnDiEdge]) extends AntTreeModelAncestor[BinaryVector, D] with ClusteringModelLocalBinary {

  val algorithmID = org.clustering4ever.roottraits.AntTreeBinary

}

final  case class AntTreeModelMixed[D <: MixedDistance](val metric: D, val tree: Tree[(Long, Option[MixedVector]), UnDiEdge]) extends AntTreeModelAncestor[MixedVector, D] {

  val algorithmID = org.clustering4ever.roottraits.AntTreeMixed

}
