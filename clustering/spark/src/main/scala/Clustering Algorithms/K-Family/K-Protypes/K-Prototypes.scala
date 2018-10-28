package clustering4ever.spark.clustering.kprototypes

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.spark.clustering.KCommonsSparkMixt
import scala.language.higherKinds
/**
 * @author Beck Gaël
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding MixtDistance distance function
 */
class KPrototypes[
	ID: Numeric,
	O,
	Vb[Int] <: Seq[Int],
	Vs[Double] <: Seq[Double],
	Cz[ID, O, Vb <: Seq[Int], Vs <: Seq[Double]] <: MixtClusterizable[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs]]
	// D <: HammingAndEuclidean[Vb[Int], Vs[Double]]
](
	@transient val sc: SparkContext,
	dataIn: RDD[Cz[ID, O, Vb[Int], Vs[Double]]],
	k: Int,
	epsilon: Double,
	maxIter: Int,
	metric: HammingAndEuclidean[Vb[Int], Vs[Double]] = new HammingAndEuclidean[Vb[Int], Vs[Double]],
	initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb[Int], Vs[Double]]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb[Int], Vs[Double]]],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
)(implicit ct: ClassTag[Cz[ID, O, Vb[Int], Vs[Double]]]) extends KCommonsSparkMixt[ID, Vb, Vs, Cz[ID, O, Vb[Int], Vs[Double]], HammingAndEuclidean[Vb[Int], Vs[Double]]](dataIn, metric, k, initializedCenters, persistanceLVL) {

	private[this] val data = dataIn.map(_.vector).persist(persistanceLVL)

	private[this] def obtainNearestModID(v: BinaryScalarVector[Vb[Int], Vs[Double]], centers: mutable.HashMap[Int, BinaryScalarVector[Vb[Int], Vs[Double]]]): Int = centers.minBy{ case(clusterID, mod) => metric.d(mod, v) }._1

	def run(): KPrototypesModel[ID, O, Vb[Int], Vs[Double], Cz[ID, O, Vb[Int], Vs[Double]], HammingAndEuclidean[Vb[Int], Vs[Double]]] = {
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged ) {
			val centersInfo = obtainvalCentersInfo.map{ case (clusterID, (cardinality, preMean)) =>
				// Majority Vote for Hamming Distance
				val binaryVector = preMean.binary.map( v => if( v * 2 > cardinality ) 1 else 0 ).asInstanceOf[Vb[Int]]
				// Mean for Euclidean Distance
				val scalarVector = preMean.scalar.map(_ / cardinality).asInstanceOf[Vs[Double]]
				(clusterID, new BinaryScalarVector(binaryVector, scalarVector), cardinality)
			}
			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KPrototypesModel[ID, O, Vb[Int], Vs[Double], Cz[ID, O, Vb[Int], Vs[Double]], HammingAndEuclidean[Vb[Int], Vs[Double]]](centers, metric)
	}
}
/**
 *
 */
object KPrototypes {
	def run[
		ID: Numeric,
		O,
		Vb[Int] <: Seq[Int],
		Vs[Double] <: Seq[Double],
		Cz[ID, O, Vb <: Seq[Int], Vs <: Seq[Double]] <: MixtClusterizable[ID, O, Vb, Vs, Cz[ID, O, Vb, Vs]]
	](
		@(transient @param) sc: SparkContext,
		data: RDD[Cz[ID, O, Vb[Int], Vs[Double]]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: HammingAndEuclidean[Vb[Int], Vs[Double]] = new HammingAndEuclidean[Vb[Int], Vs[Double]],
		initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb[Int], Vs[Double]]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb[Int], Vs[Double]]],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	)(implicit ct: ClassTag[Cz[ID, O, Vb[Int], Vs[Double]]]): KPrototypesModel[ID, O, Vb[Int], Vs[Double], Cz[ID, O, Vb[Int], Vs[Double]], HammingAndEuclidean[Vb[Int], Vs[Double]]] = {
		val kPrototypes = new KPrototypes(sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}