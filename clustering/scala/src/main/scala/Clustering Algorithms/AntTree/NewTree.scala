package org.clustering4ever.clustering.anttree

import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scala.collection.{GenSeq, mutable}


trait Tree[V <: GVector[V], D <: Distance[V]]{

  val metric: D

  final def train[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): Unit = {

    // trait AntTreeObject {
    // Possible de faire avec val mais il faudrait une méthode
    // var firstTime = true
    // def update
    // }

    class Ant(final val clusterizable: Option[Cz[O, V]], final var firstTime: Boolean = true) {
      final val id = if (clusterizable.isDefined) clusterizable.get.id else Long.MinValue
    }

    object Support extends Ant(None, true)

    val x0 = Support

    val ants = {
      val dataToAnts = data.map(cz => new Ant(Some(cz)))
      dataToAnts.map(cz => cz.id).toArray.zip(dataToAnts).toMap + (x0.id -> x0)
    }

    val notConnectedAnts = mutable.Queue(ants.keys.filter(key => key != x0.id).toSeq: _*)

    val branch: Graph[Long, UnDiEdge] = Graph[Long, UnDiEdge](x0.id)

    def allSuccessors(xpos: Long): Set[branch.NodeT] = {
      val node = branch.get(xpos)
      node.withSubgraph().toSet - node
    }

    def directSuccessors(xpos: Long): Set[branch.NodeT] = branch.get(xpos).diSuccessors

    def connect(xi: Long, xpos: Long): Unit = {
      branch += xpos ~> xi
    }

    def disconnect(xpos: Long): Unit = {
      val node = branch.get(xpos)
      val successors = allSuccessors(xpos) + node
      for (key <- successors) ants(key).firstTime = true
      branch --= successors
    }

    def dissimilarValue(xpos: Long): Double = {
      val successors = directSuccessors(xpos)
      successors.map(successor => metric.d(ants(xpos).clusterizable.get.v, ants(successor).clusterizable.get.v)).min
    }

    @annotation.tailrec
    def findxplus(xi: Long, xplus: Long, successors: Set[branch.NodeT]): Long = {
      if (successors.isEmpty) xplus
      else {
        if (metric.d(ants(xi).clusterizable.get.v, ants(successors.head).clusterizable.get.v) >
          metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v)) findxplus(xi, successors.head, successors.tail)
        else findxplus(xi, xplus, successors.tail)
      }
    }

    def mostSimilarNode(xi: Long, xpos: Long): Long = {
      val successors = directSuccessors(xpos)
      findxplus(xi, successors.head, successors.tail)
    }

    def algorithm(xi: Long, xpos: Long): Long = {
      if (directSuccessors(xpos).size < 2){
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
          } else {
            ants(xpos).firstTime = false
            xplus
          }
        } else {
          if (metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v) < tDissim) {
            connect(xi, xpos)
            -1L
          } else {
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
        place(xi, x0.id)
      }
    }

  }
}
