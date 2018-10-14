package clustering4ever.scala.clustering.kprotoypes
/**
 * @author Beck Gaël
 */
import scala.collection.{GenSeq, mutable}
import scala.util.Random
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.math.distances.{MixtDistance, MixtDistanceClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.scala.clustering.KCommonsMixt
import clustering4ever.util.CommonTypes
import scala.language.higherKinds
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KPrototypes[
	ID: Numeric,
	O,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	V <: BinaryScalarVector[Vb, Vs],
	Cz[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] <: MixtClusterizable[ID, O, Vb, Vs, V, Cz[ID, O, Vb, Vs, V]],
	D <: MixtDistance[Vb, Vs, V]
](
	data: GenSeq[Cz[ID, O, Vb, Vs, V]],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new HammingAndEuclidean[Vb, Vs, V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsMixt[ID, Vb, Vs, V, D, Cz[ID, O, Vb, Vs, V]](data, metric, k, initializedCenters) {
	/**
	 * Run the K-Protypes
	 **/
	def run(): KPrototypesModel[ID, Vb, Vs, O, V, Cz[ID, O, Vb, Vs, V], D] = {
		var cpt = 0
		var allCentersHaveConverged = false

		def runHammingAndEuclidean(): KPrototypesModel[ID, Vb, Vs, O, V, Cz[ID, O, Vb, Vs, V], D] = {
			while( cpt < maxIterations && ! allCentersHaveConverged ) {
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = new BinaryScalarVector[Vb, Vs](ClusterBasicOperations.obtainMode[Vb](aggregate.map(_._1.binary)), ClusterBasicOperations.obtainMean[Vs](aggregate.map(_._1.scalar))).asInstanceOf[V]
					centersCardinality(clusterID) += aggregate.size
				}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KPrototypesModel[ID, Vb, Vs, O, V, Cz[ID, O, Vb, Vs, V], D](centers, metric)
		}
		def runCustom(): KPrototypesModel[ID, Vb, Vs, O, V, Cz[ID, O, Vb, Vs, V], D] = {
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KPrototypesModel[ID, Vb, Vs, O, V, Cz[ID, O, Vb, Vs, V], D](centers, metric)
		}
	
		if( metric.isInstanceOf[HammingAndEuclidean[Vb, Vs, V]] ) runHammingAndEuclidean() else runCustom()
	}
}

object KPrototypes extends CommonTypes {
	/**
	 * Run the K-Prototypes with any mixt distance
	 */
	def run[
		ID: Numeric,
		O,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		V <: BSV[Vb, Vs],
		Cz[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], V <: BSV[Vb, Vs]] <: MixtClusterizable[ID, O, Vb, Vs, V, Cz[ID, O, Vb, Vs, V]],
		D <: MixtDistance[Vb, Vs, V]
	](
		data: GenSeq[Cz[ID, O, Vb, Vs, V]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	): KPrototypesModel[ID, Vb, Vs, O, V, Cz[ID, O, Vb, Vs, V], D] = {
		val kPrototypes = new KPrototypes[ID, O, Vb, Vs, V, Cz, D](data, k, epsilon, maxIterations, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}