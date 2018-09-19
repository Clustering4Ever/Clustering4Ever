package clustering4ever.spark.clustering.kmodes

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.spark.clustering.KCommonsSparkVectors

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Seq with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding BinaryDistance distance function
 **/
class KModes[
	ID: Numeric,
	O,
	V <: Seq[Int] : ClassTag,
	Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V],
	D <: Hamming[V]
](
	@transient val sc: SparkContext,
	data: RDD[Cz[ID, O, V]],
	k: Int,
	epsilon: Double,
	maxIter: Int,
	metric: D = new Hamming[V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
)(implicit ct: ClassTag[Cz[ID, O, V]]) extends KCommonsSparkVectors[ID, Int, V, Cz[ID, O, V], D](data, metric, k, initializedCenters, persistanceLVL) {
	def run(): KModesModel[ID, O, V, Cz[ID, O, V], D] =	{
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged ) {
			val centersInfo = obtainvalCentersInfo.map{ case (clusterID, (cardinality, preMode)) => 
				(
					clusterID,
					preMode.map( x => if( x * 2 >= cardinality ) 1 else 0 ).asInstanceOf[V],
					cardinality
				)
			}
			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KModesModel[ID, O, V, Cz[ID, O, V], D](centers, metric)
	}
}

object KModes
{
	def run[
		ID: Numeric,
		O,
		V <: Seq[Int] : ClassTag,
		Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V]
		// D <: Hamming[V]
	](
		@(transient @param) sc: SparkContext,
		data: RDD[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	)(implicit ct: ClassTag[Cz[ID, O, V]]): KModesModel[ID, O, V, Cz[ID, O, V], Hamming[V]] = {
		val metric = new Hamming[V]
		val kmodes = new KModes[ID, O, V, Cz, Hamming[V]](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kModesModel = kmodes.run()
		kModesModel
	}
}