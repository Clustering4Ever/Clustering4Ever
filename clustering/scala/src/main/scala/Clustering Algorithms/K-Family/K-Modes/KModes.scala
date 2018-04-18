package clustering4ever.scala.clustering.kmodes

import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.math.distances.binary.Hamming
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms

class KModes(
	data: Seq[Array[Int]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance
) extends ClusteringAlgorithms[Int, Array[Int]]
{
	val dim = data.head.size

	def run(): KModesModel =
	{
		val centers = mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, Array.fill(dim)(Random.nextInt(2))) )):_*)
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		/**
		 * Return the nearest mode for a specific point
		 **/
		def obtainNearestModID(v: Array[Int]): ClusterID =
		{
			centers.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.minBy(_._2)._1
		}

		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 **/
		def obtainMedoid(arr: Seq[Array[Int]]): Array[Int] =
		{
			(for( v1 <- arr) yield( (v1, (for( v2 <- arr ) yield(metric.d(v1, v2))).sum / arr.size) )).sortBy(_._2).head._1
		}

		val zeroMod = Array.fill(dim)(0)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < maxIter && ! allModsHaveConverged )
		{
			// Allocation to modes
			val clusterized = data.map( v => (v, obtainNearestModID(v)) )

			val kCentersBeforeUpdate = centers.clone

			// Reinitialization of modes
			centers.foreach{ case (clusterID, mod) => centers(clusterID) = zeroMod }
			centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[Hamming] )
			{
				// Updatating Modes
				clusterized.foreach{ case (v, clusterID) =>
				{
					centers(clusterID) = SumArrays.sumArraysNumerics(centers(clusterID), v)
					centersCardinality(clusterID) += 1
				}}

				centers.foreach{ case (clusterID, mod) => centers(clusterID) = mod.map( v => if( v * 2 >= centersCardinality(clusterID) ) 1 else 0 ) }
			}
			else
			{	
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregates) =>
				{
					val cluster = aggregates.map{ case (vector, _) => vector }
					val mode = obtainMedoid(cluster)
					centers(clusterID) = mode
				}}
			}

			allModsHaveConverged = kCentersBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, centers(clusterID)) <= epsilon }

			cpt += 1
		}

		new KModesModel(centers, centersCardinality, metric)
	}
	
}

object KModes extends DataSetsTypes[Int, Array[Int]]
{

	def run(data: Seq[Vector], k: Int, epsilon: Double, maxIter: Int, metric: BinaryDistance): KModesModel =
	{
		val kmodes = new KModes(data, k, epsilon, maxIter, metric)
		val kModesModel = kmodes.run()
		kModesModel
	}
}