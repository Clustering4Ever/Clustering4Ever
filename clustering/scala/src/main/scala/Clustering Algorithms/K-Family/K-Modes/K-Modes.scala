package clustering4ever.scala.clustering.kmodes
/**
 * @author Beck Gaël
 */
import scala.collection.{GenSeq, mutable}
import scala.reflect.ClassTag
import scala.util.Random
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.scala.clustering.KCommonsVectors
import clustering4ever.util.CommonTypes

class KModes[
	ID: Numeric,
	O,
	V <: Seq[Int] : ClassTag,
	Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V],
	D <: BinaryDistance[V]
](
	data: GenSeq[Cz[ID, O, V]],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new Hamming[V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Int, V, D, Cz[ID, O, V]](data, metric, k, initializedCenters) {
	/**
	 * Run the K-Means
	 */
	def run(): KModesModel[ID, O, V, Cz[ID, O, V], D] = {
		/**
		 * Run the K-Modes with Hamming metric
		 */
		def runHamming(): KModesModel[ID, O, V, Cz[ID, O, V], D] = {
			var cpt = 0
			var allCentersHaveConverged = false
			while( cpt < maxIterations && ! allCentersHaveConverged ) {
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = ClusterBasicOperations.obtainMode(aggregate.map(_._1)).asInstanceOf[V]
					centersCardinality(clusterID) += aggregate.size
				}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KModesModel[ID, O, V, Cz[ID, O, V], D](centers, metric)
		}

		def runCustom(): KModesModel[ID, O, V, Cz[ID, O, V], D] = {
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KModesModel[ID, O, V, Cz[ID, O, V], D](centers, metric)
		}
	
		if( metric.isInstanceOf[Hamming[V]] ) runHamming() else runCustom()
	}
}

object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[
		ID: Numeric,
		O,
		V <: Seq[Int] : ClassTag,
		Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V],
		D <: BinaryDistance[V]
	](
		data: GenSeq[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D = new Hamming[V],
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	): KModesModel[ID, O, V, Cz[ID, O, V], D] = {
		val kmodes = new KModes[ID, O, V, Cz, D](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}