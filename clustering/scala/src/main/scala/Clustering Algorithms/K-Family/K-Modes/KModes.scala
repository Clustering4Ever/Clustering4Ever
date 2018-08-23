package clustering4ever.scala.clustering.kmodes

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable, GenSeq, parallel}
import scala.math.pow
import scala.util.Random
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.SumArrays
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.scala.clustering.KppInitialization

class KModes[ID: Numeric, Obj](
	data: GenSeq[BinaryClusterizable[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance[Seq[Int]],
	var initializedCenters: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]]
) extends ClusteringAlgorithms[ID]
{
	private[this] val binaryDS = data.map(_.vector.toSeq).par
	private[this] val dim = binaryDS.head.size

	/**
	 * Simplest centers initialization which generate random binary vectors 
	 */
	def naiveInitialization() = mutable.HashMap((0 until k).map( clusterID => (clusterID, Seq.fill(dim)(Random.nextInt(2))) ):_*) 
	
	def run(): KModesModel =
	{


		val centers = if( initializedCenters.isEmpty ) KppInitialization.kmppInitialization[Int, Seq[Int], parallel.ParSeq[Seq[Int]]](binaryDS, k, metric) else initializedCenters
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		/**
		 * Return the nearest mode for a specific point
		 */
		def obtainNearestCenterID(v: Seq[Int]): ClusterID = centers.minBy{ case (clusterID, mode) => metric.d(mode, v) }._1

		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 */
		def obtainMedoid(gs: GenSeq[Seq[Int]]): Seq[Int] = gs.minBy( v1 => gs.map( v2 => metric.d(v1, v2) ).sum )

		/**
		 * Check if there are empty centers and remove them
		 */
		def removeEmptyClusters(kCentersBeforeUpdate: mutable.HashMap[Int, Seq[Int]]) =
		{
			// Check if there are empty centers and remove them
			val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}

		val zeroMod = Seq.fill(dim)(0)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < maxIter && ! allModsHaveConverged )
		{
			// Allocation to modes
			val clusterized = binaryDS.map( v => (v, obtainNearestCenterID(v)) )

			val kCentersBeforeUpdate = centers.clone

			// Reinitialization of modes
			centers.foreach{ case (clusterID, mode) => centers(clusterID) = zeroMod }
			centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[Hamming] )
			{
				// Update Modes and Cardinalities
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = SumArrays.obtainMode(aggregate.map(_._1))
					centersCardinality(clusterID) += aggregate.size
				}}
				removeEmptyClusters(kCentersBeforeUpdate)
			}
			else
			{	
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregates) =>
				{
					val cluster = aggregates.map{ case (vector, _) => vector }
					val mode = obtainMedoid(cluster)
					centers(clusterID) = mode
					centersCardinality(clusterID) += 1
				}}
				removeEmptyClusters(kCentersBeforeUpdate)
			}
			allModsHaveConverged = kCentersBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, centers(clusterID)) <= epsilon }
			cpt += 1
		}
		new KModesModel(centers, metric)
	}
	
}

object KModes
{

	def run[ID: Numeric, Obj](
		data: GenSeq[BinaryClusterizable[ID, Obj]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: BinaryDistance[Seq[Int]],
		initializedCenters: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]]
	): KModesModel =
	{
		val kmodes = new KModes[ID, Obj](data, k, epsilon, maxIter, metric, initializedCenters)
		val kModesModel = kmodes.run()
		kModesModel
	}
}