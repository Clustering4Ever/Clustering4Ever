package clustering4ever.scala.clustering.kprotoypes
/**
 * @author Beck Gaël
 */
import scala.collection.{GenSeq, mutable}
import scala.util.Random
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.math.distances.MixtDistance
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
	Cz[ID, O, Vb <: Seq[Int], Vs <: Seq[Double]] <: MixtClusterizable[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs]],
	D <: MixtDistance[Vb, Vs]
](
	data: GenSeq[Cz[ID, O, Vb, Vs]],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new HammingAndEuclidean[Vb, Vs],
	initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb, Vs]]
) extends KCommonsMixt[ID, Vb, Vs, BinaryScalarVector[Vb, Vs], D, Cz[ID, O, Vb, Vs]](data, metric, k, initializedCenters) {
	/**
	 * Run the K-Protypes
	 **/
	def run(): KPrototypesModel[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs], D] = {
		var cpt = 0
		var allCentersHaveConverged = false

		def runHammingAndEuclidean(): KPrototypesModel[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs], D] = {
			while( cpt < maxIterations && ! allCentersHaveConverged ) {
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = new BinaryScalarVector[Vb, Vs](ClusterBasicOperations.obtainMode(aggregate.map(_._1.binary)), ClusterBasicOperations.obtainMean(aggregate.map(_._1.scalar)))
					centersCardinality(clusterID) += aggregate.size
				}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KPrototypesModel[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs], D](centers, metric)
		}
		def runCustom(): KPrototypesModel[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs], D] = {
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KPrototypesModel[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs], D](centers, metric)
		}
	
		if( metric.isInstanceOf[HammingAndEuclidean[Vb, Vs]] ) runHammingAndEuclidean() else runCustom()
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
		Cz[ID, O, Vb <: Seq[Int], Vs <: Seq[Double]] <: MixtClusterizable[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs]],
		D <: MixtDistance[Vb, Vs]
	](
		data: GenSeq[Cz[ID, O, Vb, Vs]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb, Vs]]
	): KPrototypesModel[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs], D] = {
		val kPrototypes = new KPrototypes[ID, O, Vb, Vs, Cz, D](data, k, epsilon, maxIterations, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}