package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.MixtDistance
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, MixtVector}
/**
 *
 */
case class KPrototypes[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val args: KPrototypesArgs[Vb, Vs, D])(protected implicit val ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]], protected val ctV: ClassTag[MixtVector[Vb, Vs]]) extends KCentersAncestor[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], KPrototypesArgs[Vb, Vs, D], KPrototypesModel[ID, O, Vb, Vs, Cz, D]] {
	/**
	 *
	 */
	def run(data: RDD[Cz[ID, O, MixtVector[Vb, Vs]]]): KPrototypesModel[ID, O, Vb, Vs, Cz, D] = KPrototypesModel[ID, O, Vb, Vs, Cz, D](obtainCenters(data), args.metric, args)
}
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
		D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]
	](
		data: RDD[Cz[ID, O, MixtVector[Vb, Vs]]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D[Vb, Vs],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
		initializedCenters: immutable.HashMap[Int, MixtVector[Vb, Vs]] = immutable.HashMap.empty[Int, MixtVector[Vb, Vs]]
	)(implicit ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]): KPrototypesModel[ID, O, Vb, Vs, Cz, D] = {
		val kPrototypes = KPrototypes[ID, O, Vb, Vs, Cz, D](KPrototypesArgs(k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters))
		val kPrototypesModel = kPrototypes.run(data)
		kPrototypesModel
	}
}