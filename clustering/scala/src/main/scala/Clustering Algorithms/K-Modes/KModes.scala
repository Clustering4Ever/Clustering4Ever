package clustering4ever.scala.clustering.kmodes

import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.math.distances.binary.Hamming
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms

class KModes(
	data: Seq[(Int, Array[Int])],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance
) extends ClusteringAlgorithms[Int, Int]
{
	val dim = data.head._2.size

	def run(): KModesModel =
	{
		val modes = mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, Array.fill(dim)(Random.nextInt(2))) )):_*)
		val modesCardinality = modes.map{ case (clusterID, _) => (clusterID, 0) }

		/**
		 * Return the nearest mode for a specific point
		 **/
		def obtainNearestModID(v: Array[Int]): ClusterID =
		{
			modes.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1
		}

		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 **/
		def obtainMode(arr: Seq[Array[Int]]): Array[Int] =
		{
			(for( v1 <- arr) yield( (v1, (for( v2 <- arr ) yield(metric.d(v1, v2))).reduce(_ + _) / arr.size) )).sortBy(_._2).head._1
		}

		val zeroMod = Array.fill(dim)(0)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < maxIter && ! allModsHaveConverged )
		{
			// Allocation to modes
			val clusterized = data.map{ case (id, v) => (id, v, obtainNearestModID(v)) }

			val kModesBeforeUpdate = modes.clone

			// Reinitialization of modes
			modes.foreach{ case (clusterID, mod) => modes(clusterID) = zeroMod }
			modesCardinality.foreach{ case (clusterID, _) => modesCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[Hamming] )
			{
				// Updatating Modes
				clusterized.foreach{ case (_, v, clusterID) =>
				{
					modes(clusterID) = SumArrays.sumArraysNumerics(modes(clusterID), v)
					modesCardinality(clusterID) += 1
				}}

				modes.foreach{ case (clusterID, mod) => modes(clusterID) = mod.map( v => if( v * 2 >= modesCardinality(clusterID) ) 1 else 0 ) }
			}
			else
			{	
				clusterized.groupBy{ case (_, _, clusterID) => clusterID }.foreach{ case (clusterID, aggregates) =>
				{
					val cluster = aggregates.map{ case (_, vector, _) => vector }
					val mode = obtainMode(cluster)
					modes(clusterID) = mode
				}}
			}

			allModsHaveConverged = kModesBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, modes(clusterID)) <= epsilon }

			cpt += 1
		}

		new KModesModel(modes, modesCardinality, metric)
	}
	
}

object KModes extends DataSetsTypes[Int, Int]
{

	def run(data: Array[(ID, Vector)], k: Int, epsilon: Double, maxIter: Int, metric: BinaryDistance): KModesModel =
	{
		val kmodes = new KModes(data, k, epsilon, maxIter, metric)
		val kModesModel = kmodes.run()
		kModesModel
	}
}