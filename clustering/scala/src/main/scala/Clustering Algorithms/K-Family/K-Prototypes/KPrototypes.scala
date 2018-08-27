package clustering4ever.scala.clustering.kprotoypes

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.util.SumArrays
import clustering4ever.math.distances.{MixtDistance, MixtDistanceClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.scala.clustering.KCommonsMixt

/**
 * @author Beck Gaël
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 **/
class KPrototypes[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](
	data: GenSeq[MixtClusterizable[ID, Obj, Vb, Vs, V]],
	k: Int,
	epsilon: Double,
	iterMax: Int,
	metric: MixtDistance[Vb, Vs, V] = new HammingAndEuclidean[Vb, Vs, V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsMixt[ID, Vb, Vs, V, MixtDistance[Vb, Vs, V], MixtClusterizable[ID, Obj, Vb, Vs, V]](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Protypes
	 **/
	def run(): KPrototypesModel[ID, Vb, Vs, Obj, V] =
	{
		var cpt = 0
		var allCentersHaveConverged = false

		def runHammingAndEuclidean(): KPrototypesModel[ID, Vb, Vs, Obj, V] =
		{
			while( cpt < iterMax && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(vectorizedDataset, centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = new BinaryScalarVector[Vb, Vs](SumArrays.obtainModeGen[Vb](aggregate.map(_._1.binary)), SumArrays.obtainMeanGen[Vs](aggregate.map(_._1.scalar))).asInstanceOf[V]
					centersCardinality(clusterID) += aggregate.size
				}}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KPrototypesModel[ID, Vb, Vs, Obj, V](centers, metric)
		}
		def runCustom(): KPrototypesModel[ID, Vb, Vs, Obj, V] =
		{
			while( cpt < iterMax && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(vectorizedDataset, centers, centersCardinality)
				updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KPrototypesModel[ID, Vb, Vs, Obj, V](centers, metric)
		}
	
		if( metric.isInstanceOf[HammingAndEuclidean[Vb, Vs, V]] ) runHammingAndEuclidean() else runCustom()
	}
}

object KPrototypes
{
	/**
	 * Run the K-Protypes
	 **/
	def run[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](data: GenSeq[MixtClusterizable[ID, Obj, Vb, Vs, V]], k: Int, epsilon: Double, iterMax: Int, metric: MixtDistance[Vb, Vs, V], initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
): KPrototypesModel[ID, Vb, Vs, Obj, V] =
	{
		val kPrototypes = new KPrototypes[ID, Obj, Vb, Vs, V](data, k, epsilon, iterMax, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}