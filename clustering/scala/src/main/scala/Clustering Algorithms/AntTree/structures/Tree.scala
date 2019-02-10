package structures

import ants._
import calculations._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import scala.collection.mutable
import scala.io.Source

/** The tree is the main structure of the AntTree algorithm.
  *
  * @param dataSet The path to the file that contains the dataset.
  */
class Tree(dataSet: String) {
  // The support is the starting point of the tree.
  private val x0 = new Support
  // ants contains the list of all the objects in the tree with an id.
  val ants: Map[Int, Ant] = initAnts(dataSet) + (0 -> x0)
  val cluster: Set[Any] = ants.values.map(ant => ant.cluster).toSet
  val notConnectedAnts: mutable.Queue[Int]= {
    val keys = scala.util.Random.shuffle(ants.keys.toSeq.filter(int => int != 0))
    mutable.Queue(keys: _*)}
  // branch represent ant connections in the tree by means of a graph.
  val branch: Graph[Int, UnDiEdge] = Graph[Int,UnDiEdge](0)

  /** Create an Ant object file from a csv data.
    *
    * @param csv csv data.
    * @return A Ant object.
    */
  private def csvToAnt(csv: String): Ant = {
    val datas = csv.split(",")
    val features = datas.slice(0, datas.length - 1).map(_.toDouble).toVector
    val cluster = datas.last
    Ant(features, cluster)
  }

  /** Create a Map where each Ant object has a unique ID key.
    *
    * @param csvfile The path of the csv file.
    * @return A Map where each Ant object has a unique ID key.
    */
  private def initAnts(csvfile: String): Map[Int, Ant] = {
    val antList = (for (line <- Source.fromFile(csvfile).getLines()) yield csvToAnt(line)).toArray
    1.until(antList.length + 1).zip(antList).toMap
  }

  /** Determine all successors (direct and indirect) of a node.
    *
    * @param xpos The node.
    * @return A Set with all successors.
    */
  def allSuccessors(xpos: Int): Set[branch.NodeT] = {
    val node = branch.get(xpos)
    node.withSubgraph().toSet - node
  }

  /**  Determine direct successors of a node.
    *
    * @param xpos The node.
    * @return A Set with direct successors.
    */
  def directSuccessors(xpos: Int): Set[branch.NodeT] = branch.get(xpos).diSuccessors

  /** Adds a edge to branch.
    *
    * @param xi A node.
    * @param xpos A node.
    */
  def connect(xi: Int, xpos: Int): Unit = {
    branch += xpos ~> xi
  }

  /** Remove recursively all node connected to xpos.
    *
    * @param xpos The node has been deleted.
    */
  def disconnect(xpos: Int): Unit = {
    val node = branch.get(xpos)
    val successors = allSuccessors(xpos) + node
    for (key <- successors) ants(key).firstTime = true
    branch --= successors
  }

  /** Calculate the rate of data that do not have the same cluster as their parents.
    *
    * @param xpos The parent.
    * @return Rate of data that do not have the same cluster as their parents.
    */
  def errorRate(xpos: Int): Float = {
    val successors = allSuccessors(xpos)
    if (successors.isEmpty) return 0
    (successors.count(successor => ants(successor).cluster != ants(xpos).cluster) / successors.size) * 100
  }

  /** Calculate the dissimilar value observed on all the children of xpos.
    *
    * @param xpos The node.
    * @return The dissimilar value observed on all the children of xpos.
    */
  def dissimilarValue(xpos: Int): Double = {
    val successors = directSuccessors(xpos)
    successors.map(successor => Similarity.cosineSimilarity(ants(xpos).features, ants(successor).features)).min
  }

  /** Find the most similar node of xi with the children of xpos.
    *
    * @param xi xi.
    * @param xpos xpos.
    * @return The most similar node of xi with the children of xpos.
    */
  def mostSimilarNode(xi: Int, xpos: Int): Int = {
    val successors = directSuccessors(xpos)

    var xplus = successors.head
    for (node <- successors.tail) {
      if (Similarity.cosineSimilarity(ants(xi).features, ants(node).features) >
        Similarity.cosineSimilarity(ants(xi).features, ants(xplus).features)) xplus = node
    }
    xplus
  }

}

