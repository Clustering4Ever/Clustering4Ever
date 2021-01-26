package org.clustering4ever.clustering.anttree
/**
  * @author Waris Radji
  * @author Beck Gaël
  */
import org.clustering4ever.clusteringtraits.ClusteringAlgorithmLocal
import org.clustering4ever.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.roottraits._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MutableGraph}

import scala.collection.{GenSeq, immutable, mutable}
import scala.language.higherKinds

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

  /** Get the AntTree model
    *
    * @param data training data
    * @return the model
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
     * Ants (data) that are not yet connected to the tree
     */
    val notConnectedAnts: mutable.Queue[Long] = mutable.Queue(ants.keys.filter(key => key != support.id).toSeq: _*)
    /**
     * A tree that has ants as its node (data)
     */
    val tree: Tree[(Long, Option[V]), UnDiEdge] = Tree[(Long, Option[V]), UnDiEdge](MutableGraph[(Long, Option[V]), UnDiEdge]((support.id, None)))
    /** Get the node of a tree from its ID
      *
      * @param l the id
      * @return a node
      */
    def longToNode(l: Long): (Long, Option[V]) = {
      if (l== Long.MinValue) (Long.MinValue, None)
      else (l, Some(ants(l).clusterizable.get.v))

    }
    /** Connect a node to a another node on the tree
      *
      * @param xi node to connect
      * @param xpos position of the node to connect
      */
    def connect(xi: Long, xpos: Long): Unit = {
      tree.graph += longToNode(xpos) ~> longToNode(xi)
    }
    /** Disconnect a node from the tree with all its successors
      *
      * @param xpos node to disconnect
      */
    def disconnect(xpos: Long): Unit = {
      val node = tree.graph.get(longToNode(xpos))
      val successors = allSuccessors(longToNode(xpos), tree) + node
      successors.foreach(key => ants(key._1).firstTime = true)
      tree.graph --= successors.toSeq
    }
    /** Get the most dissimilar value of the nodes that are connected to an node
      *
      * @param xpos the node
      * @return dissimilar value
      */
    def dissimilarValue(xpos: Long): Double = {
      val successors = directSuccessors(longToNode(xpos), tree).toArray
      val couples = successors.combinations(2).toArray
      couples.map(c => metric.d(c(0)._2.get, c(1)._2.get)).min
    }
    /** Recursively determine the most similar node to a node from a list of successors
      *
      * @param xi the node
      * @param xplus current most similar node
      * @param successors remaining successors
      * @return similarity value
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
    /** Determine the most similar node to a node from a position
      *
      * @param xi
      * @param xpos the position
      * @return similarity value
      */
    def mostSimilarNode(xi: Long, xpos: Long): Long = {
      val successors = directSuccessors(longToNode(xpos), tree)
      findxplus(xi, successors.head._1, successors.tail)
    }
    /** Run the AntTree algorithm, determines whether an ants should hang on to a position or continue in the tree
      *
      * @param xi the ant
      * @param xpos the position
      * @return None if the ant is connected else Option(ant)
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
    /** Place a ant on the tree
      *
      * @param xi ant to place
      * @param xpos current position
      */
    @annotation.tailrec
    def place(xi: Long, xpos: Option[Long]): Unit = if (xpos.isDefined) place(xi, algorithm(xi, xpos.get))
    /**
     * Fit the model
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
final case class AntTree[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val metric: D[V]) extends AntTreeAncestor[V, D[V], AntTreeModel[V, D]] {

  final val algorithmID = org.clustering4ever.roottraits.AntTree

  final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): AntTreeModel[V, D] = {
    AntTreeModel(metric, obtainAntTree(data))
  }
}

final case class AntTreeScalar[D <: ContinuousDistance](final val metric: D) extends AntTreeAncestor[ScalarVector, D, AntTreeModelScalar[D]] {

  final val algorithmID = org.clustering4ever.roottraits.AntTreeScalar

  final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector]]): AntTreeModelScalar[D] = {
    AntTreeModelScalar(metric, obtainAntTree(data))
  }
}

final case class AntTreeBinary[D <: BinaryDistance](final val metric: D) extends AntTreeAncestor[BinaryVector, D, AntTreeModelBinary[D]] {

  final val algorithmID = org.clustering4ever.roottraits.AntTreeBinary

  final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, BinaryVector]]): AntTreeModelBinary[D] = {
    AntTreeModelBinary(metric, obtainAntTree(data))
  }
}

final case class AntTreeMixed[D <: MixedDistance](final val metric: D) extends AntTreeAncestor[MixedVector, D, AntTreeModelMixed[D]] {

  final val algorithmID = org.clustering4ever.roottraits.AntTreeMixed

  final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, MixedVector]]): AntTreeModelMixed[D] = {
    AntTreeModelMixed(metric, obtainAntTree(data))
  }
}