package org.clustering4ever.clustering.anttree
/**
 * @author Waris Radji
 * @author Beck Gaël
 */
import org.clustering4ever.clustering.{ClusteringAlgorithmLocal, ClusteringModelLocal}

import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{ContinuousDistance, Distance}
import org.clustering4ever.vectors.GVector

import scala.collection.GenSeq
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.collection.{immutable, mutable}
import scalax.collection.mutable.{Graph => MutableGraph}

import scala.collection.mutable.ArrayBuffer

class Tree[T, E[X] <: EdgeLikeIn[X]](node : MutableGraph[T,E]) {
  final protected[clustering] val graph = node
  final private val principalsCLuster = ArrayBuffer[graph.NodeT]()

  final def obtainPrincipalCluster(support: T): Unit = principalsCLuster ++= graph.get(support).diSuccessors

  final def getPrincipalCluster: ArrayBuffer[graph.NodeT] = principalsCLuster
}

trait AntTreeAlgoModelAncestor[V <: GVector[V], D <: Distance[V]] {

  val tree: Tree[(Long, Option[V]), UnDiEdge]

  final def allSuccessors(xpos: (Long, Option[V])): immutable.Set[(Long, Option[V])] = {
    val node = tree.graph.get(xpos)
    node.withSubgraph().map(_.value).toSet - node
  }

  final def directSuccessors(xpos: (Long, Option[V])): immutable.Set[(Long, Option[V])] = tree.graph.get(xpos).diSuccessors.map(_.value)

}


trait AntTreeModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with AntTreeAlgoModelAncestor[V, D] {

  val tree: Tree[(Long, Option[V]), UnDiEdge]

  val metric: D

  private val supportID = Long.MinValue

  protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {

    val supportChild= tree.getPrincipalCluster.map(e => allSuccessors(e))

    data.map( cz => cz.addClusterIDs(supportChild.indexWhere(_.contains((cz.id, Some(cz.v))))) ).asInstanceOf[GS[Cz[O, V]]]

  }


  protected[clustering] final def predict[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
    def predictOnePoint(point: Cz[O, V]): Int = tree.getPrincipalCluster.indexOf(tree.getPrincipalCluster.maxBy(c =>metric.d(c._2.get, point.v)))
    // à simplifier !
    data.map(cz => cz.addClusterIDs(predictOnePoint(cz))).asInstanceOf[GS[Cz[O, V]]]

  }

}
/**
 * @tparm V
 * @tparm D
 * @tparm CM
 */
trait AntTreeAncestor[V <: GVector[V], D <: Distance[V], CM <: AntTreeModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, CM] with AntTreeAlgoModelAncestor[V, D]{

  val metric: D


  final def train[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): Unit = {

    class Ant(final val clusterizable: Option[Cz[O, V]], final var firstTime: Boolean = true) {
      final val id = if(clusterizable.isDefined) clusterizable.get.id else Long.MinValue
    }

    object Support extends Ant(None, true)

    val support = Support

    val ants = {
      val dataToAnts = scala.util.Random.shuffle(data.map(cz => new Ant(Some(cz))).toIterator)
      dataToAnts.map( ant => (ant.id, ant) ).toMap + (support.id -> support)
    }

    val notConnectedAnts = mutable.Queue(ants.keys.filter(key => key != support.id).toSeq: _*)

    val tree: Tree[(Long, Option[V]), UnDiEdge] = new Tree[(Long, Option[V]), UnDiEdge](MutableGraph[(Long, Option[V]), UnDiEdge]((support.id, None)))


    def longToNode(l: Long): (Long, Option[V]) = (l, Option(ants(l).clusterizable.get.v))

    def connect(xi: Long, xpos: Long): Unit = {
      tree.graph += longToNode(xpos) ~> longToNode(xi)
    }

    def disconnect(xpos: Long): Unit = {
      val node = tree.graph.get(longToNode(xpos))
      val successors = allSuccessors(longToNode(xpos)) + node
      successors.foreach(key => ants(key._1).firstTime = true)
      tree.graph --= successors.asInstanceOf[Set[tree.graph.NodeT]]
    }

    def dissimilarValue(xpos: Long): Double = {
      val successors = directSuccessors(longToNode(xpos))
      val minSuccessor = successors.minBy(successor => metric.d(ants(xpos).clusterizable.get.v, ants(successor._1).clusterizable.get.v))
      metric.d(ants(xpos).clusterizable.get.v, ants(minSuccessor._1).clusterizable.get.v)
    }

    @annotation.tailrec
    def findxplus(xi: Long, xplus: Long, successors: Set[(Long, Option[V])]): Long = {
      if(successors.isEmpty) xplus
      else {
        if(
          metric.d(ants(xi).clusterizable.get.v, ants(successors.head._1).clusterizable.get.v)
          >
          metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v)
        ) {
          findxplus(xi, successors.head._1, successors.tail)
        }
        else {
          findxplus(xi, xplus, successors.tail)
        }
      }
    }

    def mostSimilarNode(xi: Long, xpos: Long): Long = {
      val successors = directSuccessors(longToNode(xpos))
      findxplus(xi, successors.head._1, successors.tail)
    }

    def algorithm(xi: Long, xpos: Long): Long = {
      if (directSuccessors(longToNode(xpos)).size < 2) {
        connect(xi, xpos)
        -1L
      } else {
        lazy val tDissim = dissimilarValue(xpos)
        lazy val xplus = mostSimilarNode(xi, xpos)

        if (ants(xpos).firstTime) {
          if (metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v) < tDissim) {
            disconnect(xplus)
            connect(xi, xpos)
            ants(xpos).firstTime = false
            -1L
          }
          else {
            ants(xpos).firstTime = false
            xplus
          }
        }
        else {
          if (metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v) < tDissim) {
            connect(xi, xpos)
            -1L
          }
          else {
            xplus
          }
        }
      }
    }

    @annotation.tailrec
    def place(xi: Long, xpos: Long): Long = if (xpos >= 0) place(xi, algorithm(xi, xpos)) else xpos

    def classify(): Unit = {
      while (notConnectedAnts.nonEmpty) {
        val xi = notConnectedAnts.dequeue
        place(xi, support.id)
      }
      tree.obtainPrincipalCluster(longToNode(support.id))
    }
    def classify2(): Unit = {
      // PE que c'est plus adapté si tu te ressert plus de notConnectedAnts après
      notConnectedAnts.foreach( ant => place(ant, support.id))
      tree.obtainPrincipalCluster(longToNode(support.id))
    }
    

  }
}


final case class AntTreeModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val metric: D[V], final val tree: MutableGraph[Long, UnDiEdge])
