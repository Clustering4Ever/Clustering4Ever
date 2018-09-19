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

package clustering4ever.spark.clustering.patchwork2

import org.apache.log4j.LogManager
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import collection.mutable

/**
 * A cluster is defined by the list of cells within.
 * @param id
 */
class PatchWorkCluster(private val clusterID: Int) extends Serializable {
  def getClusterID: Int = this.clusterID
  val cellsList: mutable.ListBuffer[PatchWorkCellKey] = mutable.ListBuffer.empty[PatchWorkCellKey]
}

/**
 *  //TODO http://stackoverflow.com/questions/30785615/reducebykey-with-a-byte-array-as-the-key
 * @param cellNumber
 * @param cellArray
 * @param cluster
 * Pass to case class
 */
class PatchWorkCellKey(val cellNumber: String, val cellArray: mutable.ArrayBuffer[Int], val cluster: Option[PatchWorkCluster]) extends Serializable {

  def this(p: mutable.ArrayBuffer[Int]) =  this(p.mkString(",").trim, p, None)

  def this(p: String) = this(p, mutable.ArrayBuffer(p.split(",").map(_.toInt):_*), None)

  override def equals(o: Any) = o match { 
    case that: PatchWorkCellKey => that.cellNumber.equals(cellNumber)
    case _ => false
  }
}

/**
 * A cell (or hypercube) in the n-dimensional feature space
 * @param cellNumber The ID of the cell. Array of size n.
 */
// class PatchWorkCell(val cellNumber: mutable.ArrayBuffer[Int]) extends Serializable {
//   /* Number of data points in the cell */
//   var ptsInCell: Int = 0

//   def this(p: mutable.ArrayBuffer[Double], eps: mutable.ArrayBuffer[Double]) = {
//     this(
//       if( p.size > 30 ) p.zipWithIndex.map{ case (value, idx) => math.ceil(value / eps(idx)).toInt }
//       else mutable.ArrayBuffer((0 until p.size).map( idx => math.ceil(p(idx) / eps(idx)).toInt ):_*)
//     )
//   }

//   def addPtsToCell = ptsInCell += 1

//   def mergeCell(points: Int) = {
//     ptsInCell += points
//     this
//   }

//   override def equals(o: Any) = o match {
//     case that: PatchWorkCell => that.cellNumber.equals(cellNumber)
//     case _ => false
//   }
// }

class PatchWorkModel(private val epsilon: mutable.ArrayBuffer[Double], val clusters: List[PatchWorkCluster]) extends Serializable {
  /**
   * After the clusters have been identified, determines in which cluster
   * data point belongs to.
   *
   * @param point A data point
   * @return The cluster the data point belongs to
   */
  def predict(point: mutable.ArrayBuffer[Double]): PatchWorkCluster = {
    val cellKey = new PatchWorkCellKey(point.zipWithIndex.map{ case (value, idx) => math.ceil(value / epsilon(idx)).toInt })
    val cl = clusters.filter( cluster => cluster.cellsList.contains(cellKey) )
    if (cl.isEmpty) {
      // point is noise
      val cluster = new PatchWorkCluster(-1)
      cluster.cellsList += cellKey
      cluster
    }
    else cl.head
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
class PatchWork(val epsilon: mutable.ArrayBuffer[Double], val minPts: Int, val ratio: Double, val minCell: Int) extends Serializable {
  /**
   * Runs the algorithm on the input data to build the cluster.
   * Checks if the dataset is cached.
   *
   * @param data Input dataset
   * @return The model
   */
  def run(data: RDD[mutable.ArrayBuffer[Double]]): PatchWorkModel = {
    if (data.getStorageLevel == StorageLevel.NONE) LogManager.getRootLogger.warn("The input data is not cached, which may impact performance.")
    runAlgorithm(data)
  }
  /**
   * Runs the algorithm on the input data to build the cluster.
   *
   * @param data Input dataset
   * @return The model
   */
  private def runAlgorithm(data: RDD[mutable.ArrayBuffer[Double]]): PatchWorkModel = {

    val dimRange = (0 until data.first.size)
    // creating pairs of (cellID,1), then reducing by key to list (cellId, cellCardinal)
    // use a wrapper instead of converting to string: http://stackoverflow.com/questions/30785615/reducebykey-with-a-byte-array-as-the-key
    // val cardinalsPatchwork: RDD[(mutable.ArrayBuffer[Int], Int)] = data.map( v => (v.zipWithIndex.map{ case (value, idx) => math.ceil(value / epsilon(idx)).toInt }, 1)).reduceByKey(_ + _)
    val cardinalsPatchwork: List[(mutable.ArrayBuffer[Int], Int)] = data.map( v => (dimRange.map( idx => math.ceil(v(idx) / epsilon(idx)).toInt ), 1) )
      .reduceByKeyLocally(_ + _)
      .toList.map{ case (c, card) => (mutable.ArrayBuffer(c:_*), card.toInt) }

    // creates clusters from the set of cells got from last operation
    // val clusters = createClusters(cardinalsPatchwork.collect.toList, minPts)
    val clusters = createClusters(cardinalsPatchwork, minPts)

    // returning the PatchWorkModel
    new PatchWorkModel(epsilon, clusters)
  }

  /**
   * computes all possible arrays in the neighbourhood of a given array
   * @param p
   * @return
   * TO DO work on tail rec implementation
   * @annotation.tailrec
   */  
  private def getNearCell(p: mutable.ArrayBuffer[Int]): List[mutable.ArrayBuffer[Int]] = {
    p.size match {
      case 1 => List(
        mutable.ArrayBuffer(p.head),
        mutable.ArrayBuffer(p.head - 1),
        mutable.ArrayBuffer(p.head + 1)
      )
      case _ => getNearCell(p.tail).flatMap( x => List(mutable.ArrayBuffer(p.head) ++ x, mutable.ArrayBuffer(p.head - 1) ++ x, mutable.ArrayBuffer(p.head + 1) ++ x) ).toList
    }
  }
  private def nearCells(p: mutable.ArrayBuffer[Int]): List[mutable.ArrayBuffer[Int]] = {
    val nearCellsList: mutable.ListBuffer[mutable.ArrayBuffer[Int]] = mutable.ListBuffer.empty[mutable.ArrayBuffer[Int]]
    for( i <- 0 until p.length ) {
      val tempP1 = p.clone()
      tempP1.update(i, p(i) - 1)
      nearCellsList += tempP1
      val tempP2 = p.clone()
      tempP2.update(i, p(i) + 1)
      nearCellsList += tempP2
    }
    nearCellsList.toList
  }
  /**
   * returns inner cells of cell
   * @param cell
   * @return
   */
  private def innerCells(cell: mutable.ArrayBuffer[Int]): List[mutable.ArrayBuffer[Int]] = if (cell.length < 3) getNearCell(cell) else nearCells(cell)
  /**
   * From a unique cell, expands the cluster to the nearest cells if they meet requirements
   * @param cardinalsPatchwork : set of Cells
   * @param cluster : current cluster being created
   * @param cell  : last considered cell
   * @param card : value to perform the ratio checking
   * @return
   */
  private def expandCluster(cardinalsPatchwork: List[(mutable.ArrayBuffer[Int], Int)], cluster: PatchWorkCluster, cell: mutable.ArrayBuffer[Int], card: Int): Unit = {
    // looking for all nearest cells and foreach perform
    innerCells(cell).foreach{ inCell =>
      // computes the cell key
      val inCellKey = new PatchWorkCellKey(inCell)
      // if this cell is not in the cluster already
      if ( ! cluster.cellsList.contains(inCellKey) ){
        val c = cardinalsPatchwork.filter{ case (c, _) => new PatchWorkCellKey(c).equals(inCellKey) }
        // if this cellID is in the set of cells, meaning their is at least one point in this cell
        if ( ! c.isEmpty) {
          // if this cell meets the requirements
          if ( c.head._2 >= card * ratio && c.head._2 >= minPts ){
            // we add this cell to the cluster and expand the cluster from her
            cluster.cellsList += inCellKey
            // Ratio from origin cell
            //expandCluster(cardinalsPatchwork, cluster, inCell, card)
            // Ratio from nearest cell
            expandCluster(cardinalsPatchwork, cluster, inCell, c.head._2)
          }
        }
      }
    }
    Unit
  }

  /**
   * Create clusters from the set of cells and their density
   *
   * @param cardinalsPatchwork List of cells and the numbers of points they contain
   * @param minPts Minimum number of points in a cell
   * @return The list of clusters
   */
  private def createClusters(cardinalsPatchwork: List[(mutable.ArrayBuffer[Int], Int)], minPts: Int): List[PatchWorkCluster] = {
    // initialising an empty list of cluster
    val clusterList: mutable.ListBuffer[PatchWorkCluster] = mutable.ListBuffer.empty[PatchWorkCluster]
    var id = 1
    // for each cells in the set of cells
    cardinalsPatchwork.sortBy(_._2).foreach{ case (cell, cardinality) =>
      //if this cell has enough points
      if( cardinality >= minPts ){
        val cellKey = new PatchWorkCellKey(cell)
        // if this cells is not already in a cluster
        if( clusterList.filter(cluster => cluster.cellsList.contains(cellKey)).isEmpty ){
          // we create a new cluster and expand it
          val cluster = new PatchWorkCluster(id)
          id = id + 1
          cluster.cellsList += cellKey
          expandCluster(cardinalsPatchwork, cluster, cell, cardinality)
          // once the cluster is expanded we add it to the list
          clusterList += cluster
        }
      }
    }
    // we keep only cluster that meets the minimum spatial size criteria
    clusterList.filter(cluster => cluster.cellsList.size > minCell).toList
  }

}
