package clustering4ever.scala.clustering.kprotoypes

import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.mixt.HammingAndEuclidean
import _root_.clustering4ever.util.SumArrays
import _root_.scala.math.{min, max}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.math.distances.MixtDistance
import _root_.clustering4ever.scala.measurableclass.BinaryScalarVector
import _root_.clustering4ever.stats.Stats

/**
 * @author Beck Gaël
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class KPrototypes(
	data: Seq[BinaryScalarVector],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: MixtDistance
) extends ClusteringAlgorithms[Int, BinaryScalarVector]
{
	val dimScalar = data.head.scalar.size
	val dimBinary = data.head.binary.size
	/**
	 * Simplest centroids initializations
	 * We search range for each dimension and take a random value between each range for scalar data and take a random {0, 1} for binary data
	 **/
	def initializationCenters(): mutable.HashMap[Int, BinaryScalarVector] =
	{
		val vectorRange = (0 until dimScalar).toArray

		val binaryModes = for( clusterID <- 0 until k ) yield ((clusterID, Array.fill(dimBinary)(Random.nextInt(2))))

		val (minv, maxv) = data.map( v => (v.scalar, v.scalar) ).reduce( (minMaxa, minMaxb) =>
		{
			val minAndMax = for( i <- vectorRange ) yield (Stats.obtainIthMinMax(i, minMaxa, minMaxb))
			minAndMax.unzip
		})

		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
		val scalarCenters = (0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) )
		
		mutable.HashMap(binaryModes.zip(scalarCenters).map{ case ((clusterID, binaryVector), (_, scalarVector)) => (clusterID, new BinaryScalarVector(binaryVector, scalarVector)) }:_*)
	}

	/**
	 * Run the K-Means
	 **/
	def run(): KPrototypesModel =
	{
		val centers = initializationCenters()
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: BinaryScalarVector): ClusterID =
		{
			centers.map{ case(clusterID, mode) => (clusterID, metric.d(mode, v)) }.minBy(_._2)._1
		}

		val zeroMod = new BinaryScalarVector(Array.fill(dimBinary)(0), Array.fill(dimScalar)(0D))
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			// Allocation to nearest centroid
			val clusterized = data.map( v => (v, obtainNearestModID(v)) )

			val kCentersBeforeUpdate = centers.clone

			// Reinitialization of centers
			centers.foreach{ case (clusterID, mode) => centers(clusterID) = zeroMod }
			centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[HammingAndEuclidean] )
			{
				// Updatating Modes
				clusterized.foreach{ case (v, clusterID) =>
				{
					centers(clusterID) =
					{
						new BinaryScalarVector(
							SumArrays.sumArraysNumerics(centers(clusterID).binary, v.binary),
							SumArrays.sumArraysNumerics(centers(clusterID).scalar, v.scalar)
						)
					}
					centersCardinality(clusterID) += 1
				}}
				// Check if there are empty centers and remove them
				val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
				centers --= emptyCenterIDs
				kCentersBeforeUpdate --= emptyCenterIDs
				// Update center vector
				centers.foreach{ case (clusterID, mode) => centers(clusterID) =
				{
					new BinaryScalarVector(
						mode.binary.map( v => if( v * 2 > centersCardinality(clusterID) ) 1 else 0 ),
						mode.scalar.map(_ / centersCardinality(clusterID))
					)
				}}
			}
			else
			{
				println("We have a bit of time before thinking of mixt data with custom distances")
			}

			allCentersHaveConverged = kCentersBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, centers(clusterID)) <= epsilon }

			cpt += 1
		}
		new KPrototypesModel(centers, metric)
	}
}

object KPrototypes extends DataSetsTypes[Int, BinaryScalarVector]
{
	/**
	 * Run the K-Protypes
	 **/
	def run(data: Seq[Vector], k: Int, epsilon: Double, iterMax: Int, metric: MixtDistance): KPrototypesModel =
	{
		val kPrototypes = new KPrototypes(data, k, epsilon, iterMax, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}