/**
 * PatchWork is a novel density-grid clustering algorithm for Apache Spark.
 * It has linear complexity and near linear horizontal scalability.
 * As a result, PatchWork can cluster a billion points in a few minutes only.
 *
 * MIT License (c) 2015 Computer Research Institute of Montreal
 *
 * @author Thomas Triplet <thomas.triplet@crim.ca>
 * @author Frank Gouineau
 *
 * https://github.com/crim-ca/patchwork
 */

package clustering4ever.spark.clustering.patchwork

import org.apache.log4j.LogManager
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel

import scala.collection.mutable.ListBuffer

object PatchWorkUtils {
  type DataPoint = Array[Double]
  type Epsilon = Array[Double]
}
import PatchWorkUtils._

/**
 * A cluster is defined by the list of cells within.
 * @param id
 */
class PatchWorkCluster(private var id: Int) extends Serializable {
  def getID: Int = this.id
  val cellsList: ListBuffer[PatchWorkCellKey] = new ListBuffer[PatchWorkCellKey]
}

/**
 *  //TODO http://stackoverflow.com/questions/30785615/reducebykey-with-a-byte-array-as-the-key
 * @param cellNumber
 * @param cellArray
 * @param cluster
 */
class PatchWorkCellKey(var cellNumber: String, var cellArray: Array[Int], var cluster: Option[PatchWorkCluster]) extends Serializable {
  def this(p: Array[Int]) {
    this(p.mkString(",").trim, p, None)
  }

  def this(p: String) {
    this(p, p.split(",").map(_.toInt), None)
  }

  override def equals(o: Any) = o match {
    case that: PatchWorkCellKey => that.cellNumber.equals(this.cellNumber)
    case _ => false
  }
}

/**
 * A cell (or hypercube) in the n-dimensional feature space
 * @param cellNumber The ID of the cell. Array of size n.
 */
class PatchWorkCell(val cellNumber: Array[Int]) extends Serializable {
  /* Number of data points in the cell */
  var ptsInCell: Int = 0

  def this(p: DataPoint, eps: Epsilon) {
    this(p.zipWithIndex.map(x => math.ceil(x._1 / eps(x._2)).toInt))
  }

  def addPtsToCell = this.ptsInCell += 1

  def mergeCell(points: Int): this.type = {
    this.ptsInCell += points
    this
  }

  override def equals(o: Any) = o match {
    case that: PatchWorkCell => that.cellNumber.equals(this.cellNumber)
    case _ => false
  }
}

class PatchWorkModel(private var epsilon: Epsilon,
    var cardinalsPatchwork: RDD[(String, Int)],
    var clusters: List[PatchWorkCluster]) extends Serializable {

  /**
   * After the clusters have been identified, determines in which cluster
   * data point belongs to.
   *
   * @param point A data point
   * @return The cluster the data point belongs to
   */
  def predict(point: DataPoint): PatchWorkCluster = {
    val cellKey = new PatchWorkCellKey(point.zipWithIndex.map(x => math.ceil(x._1 / this.epsilon(x._2)).toInt))
    val cl = this.clusters.filter(cluster => cluster.cellsList.contains(cellKey))
    if (cl.isEmpty) {
      // point is noise
      val cluster = new PatchWorkCluster(-1)
      cluster.cellsList.append(cellKey)
      cluster
    } else {
      cl.head
    }
  }
}

/**
 * PatchWork grid-density clustering algorithm
 *
 * @param epsilon cell size
 * @param minPts min number of points in a cell
 * @param ratio density changes between cells
 * @param minCell minimum spatial size of clusters
 */
class PatchWork(val epsilon: Epsilon, val minPts: Int, val ratio: Double, val minCell: Int) extends Serializable {

  /**
   * Runs the algorithm on the input data to build the cluster.
   * Checks if the dataset is cached.
   *
   * @param data Input dataset
   * @return The model
   */
  def run(data: RDD[DataPoint]): PatchWorkModel = {
    if (data.getStorageLevel == StorageLevel.NONE) {
      val log = LogManager.getRootLogger
      log.warn("The input data is not cached, which may impact performance.")
    }

    runAlgorithm(data)
  }

  /**
   * Runs the algorithm on the input data to build the cluster.
   *
   * @param data Input dataset
   * @return The model
   */
  private def runAlgorithm(data: RDD[DataPoint]): PatchWorkModel = {
    // creating pairs of (cellID,1), then reducing by key to list (cellId, cellCardinal)
    // use a wrapper instead of converting to string: http://stackoverflow.com/questions/30785615/reducebykey-with-a-byte-array-as-the-key
    val cardinalsPatchwork: RDD[(String, Int)] = data.map(p =>
      (p.zipWithIndex.map(x => math.ceil(x._1 / epsilon(x._2)).toInt).mkString(","), 1)
    ).reduceByKey(_ + _)

    // creates clusters from the set of cells got from last operation
    val clusters = createClusters(cardinalsPatchwork.map(x =>
      (x._1.split(",").map(_.toInt), x._2)
    ).collect().toList, minPts)

    // returning the PatchWorkModel
    new PatchWorkModel(epsilon, cardinalsPatchwork, clusters)
  }

  /**
   * computes all possible arrays in the neighbourhood of a given array
   * @param p
   * @return
   */
  def getNearCell(p: Array[Int]): List[Array[Int]] = p.size match {
    case 1 => List(Array(p.head), Array(p.head - 1), Array(p.head + 1))
    case _ => List.concat(
      getNearCell(p.tail).map(x => Array.concat(Array(p.head), x)),
      getNearCell(p.tail).map(x => Array.concat(Array(p.head - 1), x)),
      getNearCell(p.tail).map(x => Array.concat(Array(p.head + 1), x))
    )
  }

  def nearCells(p: Array[Int]): List[Array[Int]] = {
    val nearCellsList: ListBuffer[Array[Int]] = new ListBuffer[Array[Int]]
    for (i <- Range(0, p.length)) {
      val tempP1 = p.clone()
      tempP1.update(i, p(i) - 1)
      nearCellsList.append(tempP1)
      val tempP2 = p.clone()
      tempP2.update(i, p(i) + 1)
      nearCellsList.append(tempP2)
    }
    nearCellsList.toList
  }

  /**
   * returns inner cells of cell
   * @param cell
   * @return
   */
  def innerCells(cell: Array[Int]): List[Array[Int]] = {
    if (cell.length < 3) {
      getNearCell(cell)
    } else {
      nearCells(cell)
    }
  }

  /**
   * From a unique cell, expands the cluster to the nearest cells if they meet requirements
   * @param cardinalsPatchwork : set of Cells
   * @param cluster : current cluster being created
   * @param cell  : last considered cell
   * @param card : value to perform the ratio checking
   * @return
   */
  def expandCluster(cardinalsPatchwork: List[(Array[Int], Int)], cluster: PatchWorkCluster, cell: Array[Int], card: Int) {
    // looking for all nearest cells and foreach perform
    innerCells(cell).foreach { inCell =>
      // computes the cell key
      val inCellKey = new PatchWorkCellKey(inCell)
      // if this cell is not in the cluster already
      if (!cluster.cellsList.contains(inCellKey)) {
        val c = cardinalsPatchwork.filter(c => new PatchWorkCellKey(c._1).equals(inCellKey))
        // if this cellID is in the set of cells, meaning their is at least one point in this cell
        if (!c.isEmpty) {
          // if this cell meets the requirements
          if (c.head._2 >= card * this.ratio && c.head._2 >= this.minPts) {
            // we add this cell to the cluster and expand the cluster from her
            cluster.cellsList.append(inCellKey)
            // Ratio from origin cell
            //expandCluster(cardinalsPatchwork, cluster, inCell, card)
            // Ratio from nearest cell
            expandCluster(cardinalsPatchwork, cluster, inCell, c.head._2)
          }
        }
      }
    }
  }

  /**
   * Create clusters from the set of cells and their density
   *
   * @param cardinalsPatchwork List of cells and the numbers of points they contain
   * @param minPts Minimum number of points in a cell
   * @return The list of clusters
   */
  def createClusters(cardinalsPatchwork: List[(Array[Int], Int)], minPts: Int): List[PatchWorkCluster] = {
    // initialising an empty list of cluster
    val clusterList: ListBuffer[PatchWorkCluster] = new ListBuffer[PatchWorkCluster]
    var id = 1
    // for each cells in the set of cells
    cardinalsPatchwork.sortBy(_._2).foreach { cell =>
      //if this cell has enough points
      if (cell._2 >= minPts) {
        val cellKey = new PatchWorkCellKey(cell._1)
        // if this cells is not already in a cluster
        if (clusterList.filter(cluster => cluster.cellsList.contains(cellKey)).isEmpty) {
          // we create a new cluster and expand it
          val cluster = new PatchWorkCluster(id)
          id = id + 1
          cluster.cellsList.append(cellKey)
          expandCluster(cardinalsPatchwork, cluster, cell._1, cell._2)
          // once the cluster is expanded we add it to the list
          clusterList.append(cluster)
        }
      }
    }
    // we keep only cluster that meets the minimum spatial size criteria
    clusterList.filter(cluster => cluster.cellsList.size > this.minCell).toList
  }

}
