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
import scala.collection.mutable


import scalax.collection.mutable.{Graph => MutableGraph}


trait AntTreeAlgoModelAncestor[V <: GVector[V], D <: Distance[V]]{
  val tree: MutableGraph[Long, UnDiEdge]

  final def allSuccessors(xpos: Long): Set[Long] = {
    val node = tree.get(xpos)
    node.withSubgraph().map(_.asInstanceOf[Long]).toSet - node
  }

  final def directSuccessors(xpos: Long): Set[tree.NodeT] = tree.get(xpos).diSuccessors

}


trait AntTreeModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with AntTreeAlgoModelAncestor[V, D] {

  val tree: MutableGraph[Long, UnDiEdge]

  private val supportID = Long.MinValue

  protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {


    val supportChild: mutable.Buffer[Set[Long]] = directSuccessors(supportID).map(e => allSuccessors(e) + e).toBuffer

    data.map( cz => cz.addClusterIDs(supportChild.indexWhere(_.contains(cz.id))) ).asInstanceOf[GS[Cz[O, V]]]

  }

/*
  protected[clustering] final def predict[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {

  }
*/
}
// La classe en dehors peux être mieux pour de futur applications si il faut la complexifier, pour l'instant elle a juste l'idée
final class Ant2[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](final val clusterizable: Option[Cz[O, V]], final var firstTime: Boolean = true) {
    final val id = if(clusterizable.isDefined) clusterizable.get.id else Long.MinValue
}
/**
 * @tparm V
 * @tparm D
 * @tparm CM
 */
trait AntTreeAncestor[V <: GVector[V], D <: Distance[V], CM <: AntTreeModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, CM] with AntTreeAlgoModelAncestor[V, D]{

  val metric: D


  final def train[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): Unit = {

    // trait AntTreeObject {
    // Possible de faire avec val mais il faudrait une méthode
    // var firstTime = true
    // def update
    // }

    class Ant(final val clusterizable: Option[Cz[O, V]], final var firstTime: Boolean = true) {
      final val id = if(clusterizable.isDefined) clusterizable.get.id else Long.MinValue
    }

    object Support extends Ant(None, true)

    val support = Support

    val ants = {
      val dataToAnts = scala.util.Random.shuffle(data.map(cz => new Ant(Some(cz))).toIterator)
      // evites de zip quand tu peux faire direct un map ça économise des parcours de data
      // dataToAnts.map(_.id).toArray.zip(dataToAnts).toMap + (support.id -> support)
      dataToAnts.map( ant => (ant.id, ant) ).toMap + (support.id -> support)
    }

    val notConnectedAnts = mutable.Queue(ants.keys.filter(key => key != support.id).toSeq: _*)

    val tree: MutableGraph[Long, UnDiEdge] = MutableGraph[Long, UnDiEdge](support.id)

    def connect(xi: Long, xpos: Long): Unit = {
      tree += xpos ~> xi
    }

    def disconnect(xpos: Long): Unit = {
      val node = tree.get(xpos)
      val successors = allSuccessors(xpos) + node
      // for (key <- successors) ants(key).firstTime = true
      successors.foreach(key => ants(key).firstTime = true)
      tree --= successors.asInstanceOf[Set[tree.NodeT]]
    }

    def dissimilarValue(xpos: Long): Double = {
      val successors = directSuccessors(xpos)

      // successors.map(successor => metric.d(ants(xpos).clusterizable.get.v, ants(successor).clusterizable.get.v)).min
      val minSuccessor = successors.minBy(successor => metric.d(ants(xpos).clusterizable.get.v, ants(successor).clusterizable.get.v))
      metric.d(ants(xpos).clusterizable.get.v, ants(minSuccessor).clusterizable.get.v)
    }

    @annotation.tailrec
    def findxplus(xi: Long, xplus: Long, successors: Set[tree.NodeT]): Long = {
      if(successors.isEmpty) xplus
      else {
        if(
          metric.d(ants(xi).clusterizable.get.v, ants(successors.head).clusterizable.get.v)
          >
          metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v)
        ) {
          findxplus(xi, successors.head, successors.tail)
        }
        else {
          findxplus(xi, xplus, successors.tail)
        }
      }
    }

    def mostSimilarNode(xi: Long, xpos: Long): Long = {
      val successors = directSuccessors(xpos).asInstanceOf[Set[tree.NodeT]]
      findxplus(xi, successors.head, successors.tail)
    }

    def algorithm(xi: Long, xpos: Long): Long = {
      if (directSuccessors(xpos).size < 2) {
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
    }
    def classify2(): Unit = {
      // PE que c'est plus adapté si tu te ressert plus de notConnectedAnts après
      notConnectedAnts.foreach( ant => place(ant, support.id))
    }
    

  }
}


final case class AntTreeModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val metric: D[V], final val tree: MutableGraph[Long, UnDiEdge])
