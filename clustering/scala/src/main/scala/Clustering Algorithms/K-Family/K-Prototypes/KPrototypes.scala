package clustering4ever.scala.clustering.kprotoypes

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable}
import scala.util.Random
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.util.SumVectors
import clustering4ever.math.distances.{MixtDistance, MixtDistanceClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.scala.clustering.KCommonsMixt
import clustering4ever.util.CommonTypes

/**
 * @author Beck Gaël
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 **/
class KPrototypes[
	ID: Numeric,
	Obj,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	V <: BinaryScalarVector[Vb, Vs],
	Cz <: MixtClusterizable[ID, Obj, Vb, Vs, V],
	D <: MixtDistance[Vb, Vs, V]
](
	data: Seq[Cz],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new HammingAndEuclidean[Vb, Vs, V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsMixt[ID, Vb, Vs, V, D, Cz](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Protypes
	 **/
	def run(): KPrototypesModel[ID, Vb, Vs, Obj, V, Cz, D] =
	{
		var cpt = 0
		var allCentersHaveConverged = false

		def runHammingAndEuclidean(): KPrototypesModel[ID, Vb, Vs, Obj, V, Cz, D] =
		{
			while( cpt < maxIterations && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = new BinaryScalarVector[Vb, Vs](SumVectors.obtainMode[Vb](aggregate.map(_._1.binary)), SumVectors.obtainMean[Vs](aggregate.map(_._1.scalar))).asInstanceOf[V]
					centersCardinality(clusterID) += aggregate.size
				}}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KPrototypesModel[ID, Vb, Vs, Obj, V, Cz, D](centers, metric)
		}
		def runCustom(): KPrototypesModel[ID, Vb, Vs, Obj, V, Cz, D] =
		{
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KPrototypesModel[ID, Vb, Vs, Obj, V, Cz, D](centers, metric)
		}
	
		if( metric.isInstanceOf[HammingAndEuclidean[Vb, Vs, V]] ) runHammingAndEuclidean() else runCustom()
	}
}

object KPrototypes extends CommonTypes
{
	/**
	 * Run the K-Protypes with combination of Euclidean and Hamming distances  distance in a fast way
	 **/
	def run[
		Cz <: MixtClusterizable[
			Long,
			BSV[MB[Int], MB[Double]],
			MB[Int],
			MB[Double],
			BSV[MB[Int], MB[Double]]
			]
		](
			data: Seq[Cz],
			k: Int,
			epsilon: Double,
			maxIterations: Int
	): KPrototypesModel[
		Long,
		MB[Int],
		MB[Double],
		BSV[MB[Int], MB[Double]],
		BSV[MB[Int], MB[Double]],
		Cz,
		HammingAndEuclidean[
			MB[Int],
			MB[Double],
			BSV[MB[Int], MB[Double]]
		]
	] =
	{
		val metric = new HammingAndEuclidean[MB[Int], MB[Double], BSV[MB[Int], MB[Double]]]
		val initializedCenters = mutable.HashMap.empty[Int, BSV[MB[Int], MB[Double]]]
		val kPrototypes = new KPrototypes[
			Long,
			BSV[MB[Int], MB[Double]],
			MB[Int],
			MB[Double],
			BSV[MB[Int], MB[Double]],
			Cz,
			HammingAndEuclidean[MB[Int], MB[Double], BSV[MB[Int], MB[Double]]]](data, k, epsilon, maxIterations, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
	/**
	 * Run the K-Prototypes with any mixt distance
	 */
	def run[
		ID: Numeric,
		Obj,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		V <: BSV[Vb, Vs],
		Cz <: MixtClusterizable[ID, Obj, Vb, Vs, V],
		D <: MixtDistance[Vb, Vs, V]
	](
		data: Seq[Cz],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	): KPrototypesModel[ID, Vb, Vs, Obj, V, Cz, D] =
	{
		val kPrototypes = new KPrototypes[ID, Obj, Vb, Vs, V, Cz, D](data, k, epsilon, maxIterations, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}