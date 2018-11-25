package org.clustering4ever.spark.clustering.kprototypes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.util.SparkImplicits._
import org.apache.spark.rdd.RDD
import org.clustering4ever.spark.clustering.kcenters.{KCentersModel, KCenters}
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
		ID: Numeric,
		O,
		Vb[Int] <: Seq[Int],
		Vs[Double] <: Seq[Double],
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: MixtDistance[Vb[Int], Vs[Double]]
	](
		data: RDD[Cz[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]]]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		workingVector: Int = 0,
		initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb[Int], Vs[Double]]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb[Int], Vs[Double]]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]]]]): KCentersModel[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]], Cz[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]]], D] = {
		val kPrototypes = new KCenters[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]], Cz[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]]], D](k, epsilon, maxIterations, metric)
		val kPrototypesModel = kPrototypes.run(data)(workingVector)
		kPrototypesModel
	}
}