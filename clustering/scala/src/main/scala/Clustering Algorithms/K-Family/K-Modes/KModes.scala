package clustering4ever.scala.clustering.kmodes

import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.SumArrays
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.scala.clusterizables.BinaryClusterizable

class KModes[ID: Numeric, Obj](
	data: GenSeq[BinaryClusterizable[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance[immutable.Seq[Int]]
) extends ClusteringAlgorithms[Int, immutable.Seq[Int]]
{

	val realDS = data.map(_.vector)

	val dim = realDS.head.size

	def run(): KModesModel =
	{
		val centers = mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, immutable.Seq.fill(dim)(Random.nextInt(2))) )):_*)
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		/**
		 * Return the nearest mode for a specific point
		 **/
		def obtainNearestCenterID(v: immutable.Seq[Int]): ClusterID = centers.minBy{ case (clusterID, mod) => metric.d(mod, v) }._1

		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 **/
		def obtainMedoid(gs: GenSeq[immutable.Seq[Int]]): immutable.Seq[Int] = gs.map( v1 => (v1, gs.map( v2 => metric.d(v1, v2) ).sum / gs.size) ).minBy(_._2)._1

		/**
		 * Check if there are empty centers and remove them
		 **/
		def removeEmptyClusters(kCentersBeforeUpdate: mutable.HashMap[Int, immutable.Seq[Int]]) =
		{
			// Check if there are empty centers and remove them
			val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}

		val zeroMod = immutable.Seq.fill(dim)(0)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < maxIter && ! allModsHaveConverged )
		{
			// Allocation to modes
			val clusterized = realDS.map( v => (v, obtainNearestCenterID(v)) )

			val kCentersBeforeUpdate = centers.clone

			// Reinitialization of modes
			centers.foreach{ case (clusterID, mod) => centers(clusterID) = zeroMod }
			centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[Hamming] )
			{
				// Updatating Modes
				clusterized.foreach{ case (v, clusterID) =>
				{
					centers(clusterID) = SumArrays.sumArraysNumerics[Int](centers(clusterID), v)
					centersCardinality(clusterID) += 1
				}}
				removeEmptyClusters(kCentersBeforeUpdate)
				// Update center vector
				centers.foreach{ case (clusterID, mod) => centers(clusterID) = mod.map( v => if( v * 2 >= centersCardinality(clusterID) ) 1 else 0 ) }
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

object KModes extends DataSetsTypes[Int, immutable.Seq[Int]]
{

	def run[ID: Numeric, Obj](data: GenSeq[BinaryClusterizable[ID, Obj]], k: Int, epsilon: Double, maxIter: Int, metric: BinaryDistance[immutable.Seq[Int]]): KModesModel =
	{
		val kmodes = new KModes[ID, Obj](data, k, epsilon, maxIter, metric)
		val kModesModel = kmodes.run()
		kModesModel
	}
}