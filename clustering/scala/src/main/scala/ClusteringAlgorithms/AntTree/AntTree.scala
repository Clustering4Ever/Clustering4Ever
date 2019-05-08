package org.clustering4ever.clustering.anttree
/**
  * @author Waris Radji
  * @author Beck Gaël
  */
import scala.language.higherKinds
import org.clustering4ever.clustering.ClusteringAlgorithmLocal
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MutableGraph}

import scala.collection.{GenSeq, mutable, immutable}

/**
  * @tparm V
  * @tparm D
  * @tparm CM
  */
trait AntTreeAncestor[V <: GVector[V], D <: Distance[V], CM <: AntTreeModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, CM] with AntTreeAlgoModelAncestor[V, D]{
  /**
   *
   */
  val metric: D
  /**
   *
   */
  private[anttree] final def obtainAntTree[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): Tree[(Long, Option[V]), UnDiEdge] = {

    class Ant(final val clusterizable: Option[Cz[O, V]], final var firstTime: Boolean = true) {
      final val id = if(clusterizable.isDefined) clusterizable.get.id else Long.MinValue
    }

    object Support extends Ant(None, true)

    val support = Support

    val ants: immutable.Map[Long, Ant] = {
      val dataToAnts = scala.util.Random.shuffle(data.map(cz => new Ant(Some(cz))).toIterator)
      dataToAnts.map( ant => (ant.id, ant) ).toMap + (support.id -> support)
    }
    /**
     *
     */
    val notConnectedAnts: mutable.Queue[Long] = mutable.Queue(ants.keys.filter(key => key != support.id).toSeq: _*)
    /**
     *
     */
    val tree: Tree[(Long, Option[V]), UnDiEdge] = Tree[(Long, Option[V]), UnDiEdge](MutableGraph[(Long, Option[V]), UnDiEdge]((support.id, None)))
    /**
     *
     */
    def longToNode(l: Long): (Long, Option[V]) = {
      if (l== Long.MinValue) (Long.MinValue, None)
      else (l, Some(ants(l).clusterizable.get.v))

    }

    /**
     *
     */
    def connect(xi: Long, xpos: Long): Unit = {
      tree.graph += longToNode(xpos) ~> longToNode(xi)
    }
    /**
     *
     */
    def disconnect(xpos: Long): Unit = {
      val node = tree.graph.get(longToNode(xpos))
      val successors = allSuccessors(longToNode(xpos), tree) + node
      successors.foreach(key => ants(key._1).firstTime = true)
      tree.graph --= successors.toSeq
    }
    /**
     *
     */
    def dissimilarValue(xpos: Long): Double = {
      val successors = directSuccessors(longToNode(xpos), tree).toArray
      val couples = successors.combinations(2).toArray
      couples.map(c => metric.d(c(0)._2.get, c(1)._2.get)).min
    }

    /**
     *
     */
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
    /**
     *
     */
    def mostSimilarNode(xi: Long, xpos: Long): Long = {
      val successors = directSuccessors(longToNode(xpos), tree)
      findxplus(xi, successors.head._1, successors.tail)
    }
    /**
     *
     */
    def algorithm(xi: Long, xpos: Long): Option[Long] = {
      if(directSuccessors(longToNode(xpos), tree).size < 2) {
        connect(xi, xpos)
        None
      } else {
        lazy val tDissim = dissimilarValue(xpos)
        lazy val xplus = mostSimilarNode(xi, xpos)

        if(ants(xpos).firstTime) {
          if(metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v) < tDissim) {
            disconnect(xplus)
            connect(xi, xpos)
            ants(xpos).firstTime = false
            None
          }
          else {
            ants(xpos).firstTime = false
            Some(xplus)
          }
        }
        else {
          if(metric.d(ants(xi).clusterizable.get.v, ants(xplus).clusterizable.get.v) < tDissim) {
            connect(xi, xpos)
            None
          }
          else {
            Some(xplus)
          }
        }
      }
    }

    /**
     *
     */
    @annotation.tailrec
    def place(xi: Long, xpos: Option[Long]): Unit = if (xpos.isDefined) place(xi, algorithm(xi, xpos.get))
    /**
     *
     */
    def classify(): Unit = {
      while (notConnectedAnts.nonEmpty) {
        val xi = notConnectedAnts.dequeue
        place(xi, Some(support.id))
      }
      tree.obtainPrincipalCluster(longToNode(support.id))
    }
    def classify2(): Unit = {
      // PE que c'est plus adapté si tu te ressert plus de notConnectedAnts après
      notConnectedAnts.foreach( ant => place(ant, Some(support.id)))
      tree.obtainPrincipalCluster(longToNode(support.id))
    }


    classify()
    tree
  }
}
/**
 *
 */


final case class AntTreeScalar[D <: ContinuousDistance](final val metric: D) extends AntTreeAncestor[ScalarVector, D, AntTreeModelScalar[D]] {

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.AntTreeScalar

  final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector]]): AntTreeModelScalar[D] = {
    AntTreeModelScalar(metric, obtainAntTree(data))
  }

}