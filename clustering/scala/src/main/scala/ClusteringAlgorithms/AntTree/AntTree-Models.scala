package org.clustering4ever.clustering.anttree
/**
 * @author Waris Radji
 * @author Beck Gaël
 */
import org.clustering4ever.clustering.{ClusteringAlgorithmLocal, ClusteringModelLocal}

import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{ContinuousDistance, Distance}
import org.clustering4ever.vectors.{GVector, ScalarVector}

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
final case class Tree[T, E[X] <: EdgeLikeIn[X]](final val graph: MutableGraph[T,E]) {
  /**
   *
   */
  final private val principalsCLuster = mutable.ArrayBuffer[graph.NodeT]()
  /**
   *
   */
  final def obtainPrincipalCluster(support: T): Unit = principalsCLuster ++= graph.get(support).diSuccessors
  /**
   *
   */
  final def getPrincipalCluster: mutable.ArrayBuffer[graph.NodeT] = principalsCLuster
}
/**
 * @tparam V the vector nature
 * @tparam D the distance nature
 */
trait AntTreeAlgoModelAncestor[V <: GVector[V], D <: Distance[V]] {
  /**
   *
   */
  final def allSuccessors(xpos: (Long, Option[V]), tree: Tree[(Long, Option[V]), UnDiEdge]): immutable.Set[(Long, Option[V])] = {
    val node = tree.graph.get(xpos)
    node.withSubgraph().map(_.value).toSet - node
  }
  /**
   *
   */
  final def directSuccessors(xpos: (Long, Option[V]), tree: Tree[(Long, Option[V]), UnDiEdge]): immutable.Set[(Long, Option[V])] = tree.graph.get(xpos).diSuccessors.map(_.value)
}


trait AntTreeModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with MetricModel[V, D] with AntTreeAlgoModelAncestor[V, D] {
  /**
   * Des petites info sur l'objet à mettre en anglais
   */
  val tree: Tree[(Long, Option[V]), UnDiEdge]

  val metric: D // je n'ai pas mis de commentaires ici car ce champs herite de MetricModel qui à déjà une description, sinon on va l'override
  /**
   *
   */
  private final val supportID = Long.MinValue

  protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    val supportChild = tree.getPrincipalCluster.map(e => allSuccessors(e, tree))
    data.map( cz => cz.addClusterIDs(supportChild.indexWhere(_.contains((cz.id, Some(cz.v))))) ).asInstanceOf[GS[Cz[O, V]]]
  }
  /**
   *
   */
  final def predictOnePoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](point: Cz[O, V]): Int = {
    tree.getPrincipalCluster.indexOf(tree.getPrincipalCluster.maxBy( c => metric.d(c._2.get, point.v) ))
  }
  /**
   *
   */
  final def predict[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    // à simplifier !
    data.map( cz => cz.addClusterIDs(predictOnePoint(cz)) ).asInstanceOf[GS[Cz[O, V]]]

  }

}

final case class AntTreeModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val metric: D[V], final val tree: Tree[(Long, Option[ScalarVector[V]]), UnDiEdge]) extends AntTreeModelAncestor[ScalarVector[V], D[V]] {

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.AntTreeScalar

}