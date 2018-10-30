package clustering4ever.scala.clustering.kmodes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, mutable}
import scala.reflect.ClassTag
import scala.util.Random
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.scala.clustering.KCommonsVectors
import scala.language.higherKinds
/**
 *
 */
class KModes[
	ID: Numeric,
	O,
	V[Int] <: Seq[Int],
	Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V, Cz[ID, O, V]],
	D <: BinaryDistance[V[Int]]
](
	data: GenSeq[Cz[ID, O, V[Int]]],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V[Int]] = mutable.HashMap.empty[Int, V[Int]]
)(implicit ct: ClassTag[Cz[ID, O, V[Int]]], ct2: ClassTag[V[Int]]) extends KCommonsVectors[ID, Int, V[Int], D, Cz[ID, O, V[Int]]](data, metric, k, initializedCenters) {
	/**
	 * Run the K-Means
	 */
	def run(): KModesModel[ID, O, V, Cz[ID, O, V[Int]], D] = {
		/**
		 * Run the K-Modes with Hamming metric
		 */
		def runHamming(): KModesModel[ID, O, V, Cz[ID, O, V[Int]], D] = {
			var cpt = 0
			var allCentersHaveConverged = false
			while( cpt < maxIterations && ! allCentersHaveConverged ) {
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = ClusterBasicOperations.obtainMode(aggregate.map(_._1)).asInstanceOf[V[Int]]
					centersCardinality(clusterID) += aggregate.size
				}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KModesModel[ID, O, V, Cz[ID, O, V[Int]], D](centers, metric)
		}

		def runCustom(): KModesModel[ID, O, V, Cz[ID, O, V[Int]], D] = {
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KModesModel[ID, O, V, Cz[ID, O, V[Int]], D](centers, metric)
		}
	
		if( metric.isInstanceOf[Hamming[V[Int]]] ) runHamming() else runCustom()
	}
}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[
		ID: Numeric,
		O,
		V[Int] <: Seq[Int],
		Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V, Cz[ID, O, V]],
		D <: BinaryDistance[V[Int]]
	](
		data: GenSeq[Cz[ID, O, V[Int]]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V[Int]] = mutable.HashMap.empty[Int, V[Int]]
	)(implicit ct: ClassTag[Cz[ID, O, V[Int]]], ct2: ClassTag[V[Int]]): KModesModel[ID, O, V, Cz[ID, O, V[Int]], D] = {
		val kmodes = new KModes(data, k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}