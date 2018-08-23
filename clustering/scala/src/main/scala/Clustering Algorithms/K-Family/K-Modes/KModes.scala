package clustering4ever.scala.clustering.kmodes

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable, GenSeq, parallel}
import scala.math.pow
import scala.reflect.ClassTag
import scala.util.Random
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.SumArrays
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.scala.clustering.KCommonsVectors

class KModes[ID: Numeric, Obj, V <: Seq[Int] : ClassTag](
	data: GenSeq[BinaryClusterizable[ID, Obj, V]],
	k: Int,
	var epsilon: Double,
	var maxIter: Int,
	metric: BinaryDistance[V] = new Hamming[V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Int, V, BinaryDistance[V], BinaryClusterizable[ID, Obj, V]](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means
	 */
	def run(): KModesModel[ID, V, Obj] =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		/**
		 * Run the K-Modes with Hamming metric
		 */
		def runHamming(): KModesModel[ID, V, Obj] =
		{
			while( cpt < maxIter && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(vectorizedDataset, centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = SumArrays.obtainMode(aggregate.map(_._1)).asInstanceOf[V]
					centersCardinality(clusterID) += aggregate.size
				}}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KModesModel[ID, V, Obj](centers, metric)
		}

		def runCustom(): KModesModel[ID, V, Obj] =
		{
			while( cpt < maxIter && ! allCentersHaveConverged )
			{

				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(vectorizedDataset, centers, centersCardinality)
				updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
				cpt += 1
			}
			new KModesModel[ID, V, Obj](centers, metric)
		}
	
		if( metric.isInstanceOf[Hamming[V]] ) runHamming() else runCustom()
	}
}

object KModes
{
	def run[ID: Numeric, Obj, V <: Seq[Int] : ClassTag](
		data: GenSeq[BinaryClusterizable[ID, Obj, V]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: BinaryDistance[V] = new Hamming[V],
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	): KModesModel[ID, V, Obj] =
	{
		val kmodes = new KModes[ID, Obj, V](data, k, epsilon, maxIter, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}