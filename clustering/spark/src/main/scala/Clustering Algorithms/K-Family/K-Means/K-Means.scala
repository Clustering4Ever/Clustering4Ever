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
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.spark.clustering.KCommonsSparkVectors
import scala.language.higherKinds
/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : RDD of RealClusterizable
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KMeans[
	ID: Numeric,
	O,
	V[Double] <: Seq[Double],
	Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
	D <: Euclidean[V[Double]]
](
	@transient val sc: SparkContext,
	data: RDD[Cz[ID, O, V[Double]]],
	k: Int,
	epsilon: Double,
	maxIter: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V[Double]] = mutable.HashMap.empty[Int, V[Double]],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
)(implicit ct: ClassTag[Cz[ID, O, V[Double]]], ct2: ClassTag[V[Double]]) extends KCommonsSparkVectors[ID, Double, V, Cz[ID, O, V[Double]], Euclidean[V[Double]]](data, metric, k, initializedCenters, persistanceLVL) {

	def run(): KMeansModel[ID, O, V, Cz[ID, O, V[Double]], Euclidean[V[Double]]] = {
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged ) {
			val centersInfo = obtainvalCentersInfo.map{ case (clusterID, (cardinality, preMean)) => 
				(
					clusterID,
					preMean.map(_ / cardinality).asInstanceOf[V[Double]],
					cardinality
				)
			}
			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KMeansModel[ID, O, V, Cz[ID, O, V[Double]], Euclidean[V[Double]]](centers, metric)
	}
}


object KMeans {

	def run[
		ID: Numeric,
		O,
		V[Double] <: Seq[Double],
		Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
		D <: Euclidean[V[Double]]
	](
		@(transient @param) sc: SparkContext,
		data: RDD[Cz[ID, O, V[Double]]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: D,
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
		initializedCenters: mutable.HashMap[Int, V[Double]] = mutable.HashMap.empty[Int, V[Double]]
	)(implicit ct: ClassTag[Cz[ID, O, V[Double]]], ct2: ClassTag[V[Double]]): KMeansModel[ID, O, V, Cz[ID, O, V[Double]], Euclidean[V[Double]]] = {
		val kmeans = new KMeans(sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kmeansModel = kmeans.run()
		kmeansModel
	}
}