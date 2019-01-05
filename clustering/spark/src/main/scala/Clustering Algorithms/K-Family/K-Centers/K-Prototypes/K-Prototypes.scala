package org.clustering4ever.spark.clustering.kprototypes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.MixtDistance
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.spark.clustering.kcenters.{KCentersArgs, KCentersModel, KCenters}
import org.clustering4ever.scala.vectors.{GVector, MixtVector}
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data :
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
object KPrototypes {
	/**
	 * Run the K-Prototypes with any mixt distance
	 */
	def run[
		ID,
		O,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D <: MixtDistance[Vb, Vs]
	](
		data: RDD[Cz[ID, O, MixtVector[Vb, Vs]]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
		initializedCenters: mutable.HashMap[Int, MixtVector[Vb, Vs]] = mutable.HashMap.empty[Int, MixtVector[Vb, Vs]]
	)(implicit ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]): KCentersModel[MixtVector[Vb, Vs], D] = {
		val kPrototypes = new KCenters[MixtVector[Vb, Vs], D](new KCentersArgs[MixtVector[Vb, Vs], D](k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters))
		val kPrototypesModel = kPrototypes.run(data)
		kPrototypesModel
	}
}