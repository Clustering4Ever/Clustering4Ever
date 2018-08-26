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

class KModes[ID: Numeric, Obj](
	data: GenSeq[BinaryClusterizable[ID, Obj, Seq[Int]]],
	k: Int,
	var epsilon: Double,
	var maxIter: Int,
	metric: Hamming = new Hamming,
	initializedCenters: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]]
) extends KCommonsVectors[ID, Int, Seq[Int], BinaryDistance[Seq[Int]], BinaryClusterizable[ID, Obj, Seq[Int]]](data, metric, k, initializedCenters)
{		
	def run(): KModesModelSeq[ID, Obj] =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < maxIter && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters(vectorizedDataset, centers)
			resetCentersCardinality(centersCardinality)
			// Update Modes and Cardinalities
			clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
			{
				centers(clusterID) = SumArrays.obtainMode(aggregate.map(_._1))
				centersCardinality(clusterID) += aggregate.size
			}}
			removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			cpt += 1
		}
		new KModesModelSeq[ID, Obj](centers, metric)
	}
	
}

object KModes
{
	def run[ID: Numeric, Obj](
		data: GenSeq[BinaryClusterizable[ID, Obj, Seq[Int]]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		initializedCenters: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]]
	): KModesModelSeq[ID, Obj] =
	{
		val hammingMetric = new Hamming
		val kmodes = new KModes[ID, Obj](data, k, epsilon, maxIter, hammingMetric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}

class KModesCustom[ID: Numeric, Obj, V <: Seq[Int] : ClassTag](
	data: GenSeq[BinaryClusterizable[ID, Obj, V]],
	k: Int,
	var epsilon: Double,
	var maxIter: Int,
	metric: BinaryDistance[V],
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Int, V, BinaryDistance[V], BinaryClusterizable[ID, Obj, V]](data, metric, k, initializedCenters)
{	
	def run(): KModesModelCustom[ID, V, Obj] =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < maxIter && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters(vectorizedDataset, centers)
			resetCentersCardinality(centersCardinality)
			updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
			removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			cpt += 1
		}
		new KModesModelCustom[ID, V, Obj](centers, metric)
	}
	
}

object KModesCustom
{
	def run[ID: Numeric, Obj, V <: Seq[Int] : ClassTag](
		data: GenSeq[BinaryClusterizable[ID, Obj, V]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: BinaryDistance[V],
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	): KModesModelCustom[ID, V, Obj] =
	{
		val kmodes = new KModesCustom[ID, Obj, V](data, k, epsilon, maxIter, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}