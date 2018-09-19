package clustering4ever.spark.clustering.kmeans

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.clustering.DataSetsTypes
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.spark.clustering.KCommonsSparkVectors

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : RDD of RealClusterizable
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 **/
class KMeans[
	ID: Numeric,
	O,
	V <: Seq[Double] : ClassTag,
	Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V],
	D <: Euclidean[V]
](
	@transient val sc: SparkContext,
	data: RDD[Cz[ID, O, V]],
	k: Int,
	epsilon: Double,
	maxIter: Int,
	metric: D = new Euclidean[V](squareRoot = true),
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
)(implicit ct: ClassTag[Cz[ID, O, V]]) extends KCommonsSparkVectors[ID, Double, V, Cz[ID, O, V], D](data, metric, k, initializedCenters, persistanceLVL) {

	def run(): KMeansModel[ID, O, V, Cz[ID, O, V], D] = {
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged ) {
			val centersInfo = obtainvalCentersInfo.map{ case (clusterID, (cardinality, preMean)) => 
				(
					clusterID,
					preMean.map(_ / cardinality).asInstanceOf[V],
					cardinality
				)
			}
			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KMeansModel[ID, O, V, Cz[ID, O, V], D](centers, metric)
	}
}


object KMeans {

	def run[
		ID: Numeric,
		O,
		V <: Seq[Double] : ClassTag,
		Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V]
		// D <: Euclidean[V]
	](
		@(transient @param) sc: SparkContext,
		data: RDD[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	)(implicit ct: ClassTag[Cz[ID, O, V]]): KMeansModel[ID, O, V, Cz[ID, O, V], Euclidean[V]] = {
		val metric = new Euclidean[V](squareRoot = true)
		val kmeans = new KMeans[ID, O, V, Cz, Euclidean[V]](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kmeansModel = kmeans.run()
		kmeansModel
	}
}