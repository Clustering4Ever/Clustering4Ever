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
import clustering4ever.scala.clusterizables.ClusterizableM
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
class KPrototypes[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double]](
	data: GenSeq[ClusterizableM[ID, Obj, Vb, Vs]],
	k: Int,
	var epsilon: Double,
	var iterMax: Int,
	metric: MixtDistance[Vb, Vs],
	initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb, Vs]]
) extends KCommonsMixt[ID, Vb, Vs, BinaryScalarVector[Vb, Vs], MixtDistance[Vb, Vs], ClusterizableM[ID, Obj, Vb, Vs]](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means
	 **/
	def run(): KPrototypesModel[Vb, Vs] =
	{
		def obtainNearestModID(v: BinaryScalarVector[Vb, Vs]): ClusterID = centers.minBy{ case(clusterID, mode) => metric.d(mode, v) }._1

		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters(vectorizedDataset, centers)
			resetCentersCardinality(centersCardinality)
			if( metric.isInstanceOf[HammingAndEuclidean[Vb, Vs]] )
			{

				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = new BinaryScalarVector[Vb, Vs](SumArrays.obtainMode(aggregate.map(_._1.binary)), SumArrays.obtainMean(aggregate.map(_._1.scalar)))
					centersCardinality(clusterID) += aggregate.size
				}}
			}
			else updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)

			removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			cpt += 1
		}
		new KPrototypesModel[Vb, Vs](centers, metric)
	}
}

object KPrototypes
{
	/**
	 * Run the K-Protypes
	 **/
	def run[ID: Numeric, Obj, Vb <: Seq[Int], Vs <: Seq[Double]](data: GenSeq[ClusterizableM[ID, Obj, Vb, Vs]], k: Int, epsilon: Double, iterMax: Int, metric: MixtDistance[Vb, Vs], initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb, Vs]]
): KPrototypesModel[Vb, Vs] =
	{
		val kPrototypes = new KPrototypes[ID, Obj, Vb, Vs](data, k, epsilon, iterMax, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}