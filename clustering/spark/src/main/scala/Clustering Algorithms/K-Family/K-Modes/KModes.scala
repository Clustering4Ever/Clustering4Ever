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
import clustering4ever.util.SumVectors
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
	Obj,
	V <: Seq[Int] : ClassTag,
	Cz <: BinaryClusterizable[ID, Obj, V] : ClassTag,
	D <: Hamming[V]
](
	@transient val sc: SparkContext,
	data: RDD[Cz],
	k: Int,
	var epsilon: Double,
	var maxIter: Int,
	metric: D = new Hamming[V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends KCommonsSparkVectors[ID, Int, V, Cz, D](data, metric, k, initializedCenters, persistanceLVL)
{
	def run(): KModesModel[ID, Obj, V, Cz, D] =
	{
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
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
		new KModesModel[ID, Obj, V, Cz, D](centers, metric)
	}
}

object KModes
{
	def run[
		ID: Numeric,
		Obj,
		V <: Seq[Int] : ClassTag,
		Cz <: BinaryClusterizable[ID, Obj, V] : ClassTag
		// D <: Hamming[V]
	](
		@(transient @param) sc: SparkContext,
		data: RDD[Cz],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	): KModesModel[ID, Obj, V, Cz, Hamming[V]] =
	{
		val metric = new Hamming[V]
		val kmodes = new KModes[ID, Obj, V, Cz, Hamming[V]](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kModesModel = kmodes.run()
		kModesModel
	}
}