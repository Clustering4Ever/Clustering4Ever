package org.clustering4ever.clustering.anttree
/**
 * @author Waris Radji
 * @author Beck Gaël
 */
import org.clustering4ever.clustering.{ClusteringModelLocal, ClusteringModelLocalBinary, ClusteringModelLocalScalar}

import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.vectors.{BinaryVector, GVector, MixedVector, ScalarVector}

import scala.collection.GenSeq
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import org.clustering4ever.clustering.models.MetricModel

import scala.collection.{immutable, mutable}
import scalax.collection.mutable.{Graph => MutableGraph}
/**
 * @tparam T une description des paramètres génériques
 * @tparam E une description des paramètres génériques
 */
final case class Tree[T, E[X] <: EdgeLikeIn[X]](final val graph: MutableGraph[T, E]) {
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

  protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
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
final case class AntTreeModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val metric: D[V], final val tree: Tree[(Long, Option[V]), UnDiEdge]) extends AntTreeModelAncestor[V, D[V]]{

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.AntTree

}

final case class AntTreeModelScalar[D <: ContinuousDistance](final val metric: D, final val tree: Tree[(Long, Option[ScalarVector]), UnDiEdge]) extends AntTreeModelAncestor[ScalarVector, D] with ClusteringModelLocalScalar{

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.AntTreeScalar

}

final case class AntTreeModelBinary[D <: BinaryDistance](final val metric: D, final val tree: Tree[(Long, Option[BinaryVector]), UnDiEdge]) extends AntTreeModelAncestor[BinaryVector, D] with ClusteringModelLocalBinary {

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.AntTreeBinary

}

final  case class AntTreeModelMixed[D <: MixedDistance](final val metric: D, final val tree: Tree[(Long, Option[MixedVector]), UnDiEdge]) extends AntTreeModelAncestor[MixedVector, D] {

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.AntTreeMixed

}
