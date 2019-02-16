import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import scala.collection.{GenSeq, mutable}


trait Tree[V <: GVector[V], D <: Distance[V]]{

  val metric: D

  def train[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): Unit ={

    trait AntTreeObject {var firstTime = true}

    case class Ant(clusterizable: Cz[O, V]) extends AntTreeObject

    object Support extends Ant()

    val x0 = Support

    val ants: Map[Int, Ant] = (1 to (data.size + 1)).zip(data.map(v => Ant(v))).toMap

    val notConnectedAnts = mutable.Queue(ants.keys.toSeq: _*)

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
      for (key <- successors) ants(key).firstTime = true
      branch --= successors
    }

    def dissimilarValue(xpos: Int): Double = {
      val successors = directSuccessors(xpos)
      successors.map(successor => metric.d(ants(xpos).clusterizable.v, ants(successor).clusterizable.v)).min
    }

    def mostSimilarNode(xi: Int, xpos: Int): Int = {
      val successors = directSuccessors(xpos)

      var xplus = successors.head
      for (node <- successors.tail) {
        if (metric.d(ants(xi).clusterizable.v, ants(node).clusterizable.v) >
        metric.d(ants(xi).clusterizable.v, ants(node).clusterizable.v)) xplus = node
      }
      xplus
    }

  }

}
