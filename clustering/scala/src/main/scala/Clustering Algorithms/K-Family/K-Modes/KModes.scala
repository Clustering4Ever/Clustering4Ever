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
import clustering4ever.util.CommonTypes

class KModes[ID: Numeric, Obj, V <: Seq[Int] : ClassTag, Bc <: BinaryClusterizable[ID, Obj, V], D <: BinaryDistance[V]](
	data: GenSeq[Bc],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new Hamming[V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Int, V, D, Bc](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means
	 */
	def run(): KModesModel[ID, Obj, V, Bc, D] =
	{
		/**
		 * Run the K-Modes with Hamming metric
		 */
		def runHamming(): KModesModel[ID, Obj, V, Bc, D] =
		{
			var cpt = 0
			var allCentersHaveConverged = false
			while( cpt < maxIterations && ! allCentersHaveConverged )
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
			new KModesModel[ID, Obj, V, Bc, D](centers, metric)
		}

		def runCustom(): KModesModel[ID, Obj, V, Bc, D] =
		{
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KModesModel[ID, Obj, V, Bc, D](centers, metric)
		}
	
		if( metric.isInstanceOf[Hamming[V]] ) runHamming() else runCustom()
	}
}

object KModes extends CommonTypes
{
	/**
	 * Run the K-Modes with Hamming distance in its fastest way
	 */
	def run[Bc <: BinaryClusterizable[Long, MB[Int], MB[Int]]](
		data: GenSeq[Bc],
		k: Int,
		epsilon: Double,
		maxIterations: Int
	): KModesModel[Long, MB[Int], MB[Int], Bc, Hamming[MB[Int]]] =
	{
		val metric = new Hamming[MB[Int]]
		val initializedCenters =  mutable.HashMap.empty[Int, MB[Int]]
		val kmodes = new KModes[Long, MB[Int], MB[Int], Bc, Hamming[MB[Int]]](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[ID: Numeric, Obj, V <: Seq[Int] : ClassTag, Bc <: BinaryClusterizable[ID, Obj, V], D <: BinaryDistance[V]](
		data: GenSeq[Bc],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D = new Hamming[V],
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, MB[Int]]
	): KModesModel[ID, Obj, V, Bc, D] =
	{
		val kmodes = new KModes[ID, Obj, V, Bc, D](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}