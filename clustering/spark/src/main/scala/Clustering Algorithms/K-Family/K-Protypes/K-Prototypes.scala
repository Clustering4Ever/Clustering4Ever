package clustering4ever.spark.clustering.kprototypes

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.util.SumVectors
import clustering4ever.clustering.DataSetsTypes
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.spark.clustering.KCommonsSparkMixt

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding MixtDistance distance function
 **/
class KPrototypes[
	ID: Numeric,
	Obj,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	V <: BinaryScalarVector[Vb, Vs] : ClassTag,
	Cz <: MixtClusterizable[ID, Obj, Vb, Vs, V] : ClassTag,
	D <: HammingAndEuclidean[Vb, Vs, V]
](
	@transient val sc: SparkContext,
	dataIn: RDD[Cz],
	k: Int,
	epsilon: Double,
	maxIter: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends KCommonsSparkMixt[ID, Vb, Vs, V, Cz, D](dataIn, metric, k, initializedCenters, persistanceLVL) {

	private[this] val data = dataIn.map(_.vector).persist(persistanceLVL)

	private[this] def obtainNearestModID(v: V, centers: mutable.HashMap[Int, V]): Int = centers.minBy{ case(clusterID, mod) => metric.d(mod, v) }._1

	def run(): KPrototypesModel[ID, Obj, Vb, Vs, V, Cz, D] = {
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged ) {
			val centersInfo = obtainvalCentersInfo.map{ case (clusterID, (cardinality, preMean)) =>
				// Majority Vote for Hamming Distance
				val binaryVector = preMean.binary.map( v => if( v * 2 > cardinality ) 1 else 0 ).asInstanceOf[Vb]
				// Mean for Euclidean Distance
				val scalarVector = preMean.scalar.map(_ / cardinality).asInstanceOf[Vs]
				(clusterID, (new BinaryScalarVector[Vb, Vs](binaryVector, scalarVector)).asInstanceOf[V], cardinality)
			}
			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KPrototypesModel[ID, Obj, Vb, Vs, V, Cz, D](centers, metric)
	}
}


object KPrototypes extends DataSetsTypes[Long] {
	def run[
		ID: Numeric,
		Obj,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		V <: BinaryScalarVector[Vb, Vs] : ClassTag,
		Cz <: MixtClusterizable[ID, Obj, Vb, Vs, V] : ClassTag
		// D <: HammingAndEuclidean[Vb, Vs, V]
	](
		@(transient @param) sc: SparkContext,
		data: RDD[Cz],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	): KPrototypesModel[ID, Obj, Vb, Vs, V, Cz, HammingAndEuclidean[Vb, Vs, V]] = {
		val metric = new HammingAndEuclidean[Vb, Vs, V]
		val kPrototypes = new KPrototypes[ID, Obj, Vb, Vs, V, Cz, HammingAndEuclidean[Vb, Vs, V]](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}