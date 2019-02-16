package org.clustering4ever.clustering.anttree

import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scala.collection.{GenSeq, mutable, immutable}


trait Tree[V <: GVector[V], D <: Distance[V]]{

  val metric: D

  final def train[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): Unit ={

    // trait AntTreeObject {
      // Possible de faire avec val mais il faudrait une méthode
      // var firstTime = true
      // def update
    // }

    class Ant(val clusterizable: Option[Cz[O, V]], var firstTime: Boolean = true)// extends AntTreeObject

    object Support extends Ant(None, true)

    val x0 = Support

    val ants: immutable.Map[Long, Ant] = (data.map(cz => cz.id)).zip(data.map( cz => new Ant(Some(cz)))).toMap + (0L -> x0)

    val notConnectedAnts = mutable.Queue(ants.keys.filter(key => key != 0).toSeq:_*)

    val branch: Graph[Int, UnDiEdge] = Graph[Int,UnDiEdge](0)

    def allSuccessors(xpos: Int): Set[branch.NodeT] = {
      val node = branch.get(xpos)
      node.withSubgraph().toSet - node
    }

    def directSuccessors(xpos: Int): Set[branch.NodeT] = branch.get(xpos).diSuccessors

    def connect(xi: Int, xpos: Int): Unit = {
      branch += xpos ~> xi
    }

    def disconnect(xpos: Int): Unit = {
      val node = branch.get(xpos)
      val successors = allSuccessors(xpos) + node
      // for (key <- successors) ants(key).firstTime = true
      successors.foreach( key => ants(key).firstTime = true )
      branch --= successors
    }

    def dissimilarValue(xpos: Int): Double = {
      val successors = directSuccessors(xpos)
      successors.map(successor => metric.d(ants(xpos).clusterizable.get.v, ants(successor).clusterizable.get.v)).min
    }

    def mostSimilarNode(xi: Int, xpos: Int): Int = {
      val successors = directSuccessors(xpos)

      // tail recursion ici
      var xplus = successors.head
      for (node <- successors.tail) {
        if(metric.d(ants(xi).clusterizable.get.v, ants(node).clusterizable.get.v) > metric.d(ants(xi).clusterizable.get.v, ants(node).clusterizable.get.v)) xplus = node
      }
      // val xplus = go(...)
      xplus
    }

  }

}
