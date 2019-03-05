package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
final case class KMeans[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]])(protected implicit final val ctV: ClassTag[ScalarVector[V]]) extends KCentersAncestor[ScalarVector[V], D[V], KMeansModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): KMeansModel[V, D] = KMeansModel[V, D](k, metric, minShift, maxIterations, persistanceLVL, obtainMedians(data))
}
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
object KMeans {
	/**
	 * Run the K-Means with any continuous distance
	 */
	final def fit[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](
		data: RDD[V],
		k: Int,
		metric: D[V],
		minShift: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
		): KMeansModel[V, D] = {
		KMeans(k, metric, minShift, maxIterations, persistanceLVL).fit(scalarDataWithIDToClusterizable(data.zipWithIndex))
	}
}