package clustering4ever.scala.clustering
/**
 * @author Beck Gaël
 */
import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable, parallel, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import spire.math.{Numeric => SNumeric}
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.{ClusterizableExt, Clusterizable}
import clustering4ever.clustering.{ClusteringAlgorithms, CentersBasedModel}
import clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
abstract class KCommons[@specialized(Int, Long) ID: Numeric, V, D <: Distance[V]](metric: D) extends ClusteringAlgorithms {
	/**
	 * Check if there are empty centers and remove them
	 */
	protected def removeEmptyClusters(centers: mutable.HashMap[Int, V], kCentersBeforeUpdate: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]): Unit = {
		val emptyCenterIDs = centersCardinality.collect{ case (clusterID, cardinality) if( cardinality == 0 ) => clusterID }
		if( ! emptyCenterIDs.isEmpty ) {
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}
	}
	/**
	 * Reinitialization of cardinalities
	 */
	protected def resetCentersCardinality(centersCardinality: mutable.HashMap[Int, Int]) = centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }
	/**
	 *
	 */
	protected def obtainNearestCenterID(v: V, centers: mutable.HashMap[Int, V]): ClusterID = centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	/**
	 *
	 */
	protected def clusterizedAndSaveCenters(vectorizedDataset: GenSeq[V], centers: mutable.HashMap[Int, V]): (GenSeq[(V, Int)], mutable.HashMap[Int, V]) = {
		// Allocation to nearest centroid
		val clusterized = vectorizedDataset.map( v => (v, obtainNearestCenterID(v, centers)) )
		// Keep old position of centroids
		val kCentersBeforeUpdate = centers.clone
		(clusterized, kCentersBeforeUpdate)
	}
	
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersMovingEnough(kCentersBeforeUpdate: mutable.HashMap[Int, V], centers: mutable.HashMap[Int, V], epsilon: Double) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }

	protected def removeEmptyClustersAndCheckIfallCentersHaveConverged(centers: mutable.HashMap[Int, V], kCentersBeforeUpdate: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int], epsilon: Double): Boolean = {
		removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
		areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
	}
	/**
	 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
	 */
	protected def obtainMedoid(gs: GenSeq[V]): V = {
		gs.minBy{ v1 =>
			var sum = 0D
			gs.foreach( v2 => sum += metric.d(v1, v2) )
			sum
		}
	}
	/**
	 * Kmeans++ initialization
	 * <h2>References</h2>
	 * <ol>
	 * <li> Tapas Kanungo, David M. Mount, Nathan S. Netanyahu, Christine D. Piatko, Ruth Silverman, and Angela Y. Wu. An Efficient k-Means Clustering Algorithm: Analysis and Implementation. IEEE TRANS. PAMI, 2002.</li>
	 * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
	 * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
	 * </ol>
	 */
	lazy val kmppInitialization: (GenSeq[V], Int) => mutable.HashMap[Int, V] = (vectorizedDataset, k) => KppInitialization.kmppInitialization[V, GenSeq[V]](vectorizedDataset, k, metric)
	
}
/**
 *
 */
abstract class KCommonsScala[
	ID: Numeric,
	V,
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: GenSeq[Cz],
	metric: D,
	k: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[ID, V, D](metric) {

	val vectorizedDataset: GenSeq[V] = data.map(_.vector)
	val centers: mutable.HashMap[Int, V] = if( initializedCenters.isEmpty ) kmppInitialization(vectorizedDataset, k) else initializedCenters
	val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
	protected def clusterizedAndSaveCentersWithResetingCentersCardinalities(centers: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]): (GenSeq[(V, Int)], mutable.HashMap[Int, V]) = {
		resetCentersCardinality(centersCardinality)
		clusterizedAndSaveCenters(vectorizedDataset, centers)
	}
	/**
	 * Update Center and Cardinalities
	 */
	protected def updateCentersAndCardinalitiesCustom(clusterized: GenSeq[(V, Int)], centers: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]) = {		
		clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
			centers(clusterID) = obtainMedoid(aggregate.map(_._1))
			centersCardinality(clusterID) += aggregate.size
		}
	}

	protected def runKAlgorithmWithCustomMetric(maxIterations: Int, epsilon: Double) = {
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < maxIterations && ! allCentersHaveConverged ) {
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
			updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
			allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
			cpt += 1
		}
	}
}
/**
 *
 */
abstract class KCommonsVectors[
	ID: Numeric,
	@specialized(Int, Double) N: SNumeric,
	V <: Seq[N],
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: GenSeq[Cz],
	metric: D,
	k: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsScala[ID, V, D, Cz](data, metric, k, initializedCenters) {
	protected val dim = vectorizedDataset.head.size
}
/**
 *
 */
abstract class KCommonsMixt[
	ID: Numeric,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	V <: BinaryScalarVector[Vb, Vs],
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: GenSeq[Cz],
	metric: D,
	k: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsScala[ID, V, D, Cz](data, metric, k, initializedCenters) {

	protected val dimBinary = vectorizedDataset.head.binary.size
	protected val dimScalar = vectorizedDataset.head.scalar.size
}
/**
 *
 */
abstract class KCommonsModel[
	ID: Numeric,
	V,
	D <: Distance[V],
	Cz <: ClusterizableExt[ID, V, Cz]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D
) extends CentersBasedModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: GenSeq[Cz])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.setClusterID(centerPredict(rc.vector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[(ClusterID, V)])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.setClusterID(knnPredict(rc.vector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[Cz]): GenSeq[Cz] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID.get, rc.vector) ))
}