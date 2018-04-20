package clustering4ever.spark.clustering.kprototypes

import scala.collection.{immutable, mutable}
import scala.util.Random
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.spark.clustering.accumulators.{CentroidsScalarAccumulator, CardinalitiesAccumulator}
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.math.distances.mixt.HammingAndEuclidean
import _root_.clustering4ever.math.distances.MixtDistance
import _root_.clustering4ever.scala.measurableclass.BinaryScalarVector
import _root_.clustering4ever.stats.Stats

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding MixtDistance distance function
 **/
class KPrototypes(
	@transient val sc: SparkContext,
	data: RDD[BinaryScalarVector],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: MixtDistance
) extends ClusteringAlgorithms[Long, BinaryScalarVector]
{
	type CentersMap = mutable.HashMap[Int, BinaryScalarVector]

	def obtainNearestModID(v: BinaryScalarVector, centers: CentersMap): Int =
	{
		centers.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.minBy(_._2)._1
	}

	def run(): KPrototypesModel =
	{
		val dimScalar = data.first.scalar.size
		val dimBinary = data.first.binary.size
		
		def initializationCenters() =
		{
			val vectorRange = (0 until dimScalar).toArray
			val binaryModes = for( clusterID <- 0 until k ) yield( (clusterID, Array.fill(dimBinary)(Random.nextInt(2))) )

			def obtainMinMax(idx: Int, vminMax1: (Array[Double], Array[Double]), vminMax2: (Array[Double], Array[Double])) =
			{
				(
					min(vminMax1._1(idx), vminMax2._1(idx)),
					max(vminMax1._2(idx), vminMax2._2(idx))
				)
			}

			val (minv, maxv) = data.map( v => (v.scalar, v.scalar) ).reduce( (minMaxa, minMaxb) =>
			{
				val minAndMax = for( i <- vectorRange ) yield( obtainMinMax(i, minMaxa, minMaxb) )
				minAndMax.unzip
			})

			val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
			val scalarCentroids = (0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) )

			mutable.HashMap(binaryModes.zip(scalarCentroids).map{ case ((clusterID, binaryVector), (_, scalarVector)) => (clusterID, new BinaryScalarVector(binaryVector, scalarVector)) }:_*)
		}
		
		val centers = initializationCenters()
		val centersUpdated = centers.clone
		val clustersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[HammingAndEuclidean] )
			{
				val info = data.map( v => (obtainNearestModID(v, centers), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) =>
				{
					(
						sum1 + sum2,
						{
							val binaryVector = SumArrays.sumArraysNumerics(v1.binary, v2.binary)
							val scalarVector = SumArrays.sumArraysNumerics(v1.scalar, v2.scalar)
							new BinaryScalarVector(binaryVector, scalarVector)
						}
					)
				}}.map{ case (clusterID, (cardinality, preMean)) =>
				{
					(
						clusterID,
						{
							// Majority Vote for Hamming Distance
							val binaryVector = preMean.binary.map( v => if( v * 2 > cardinality ) 1 else 0 )
							// Mean for Euclidean Distance
							val scalarVector = preMean.scalar.map(_ / cardinality)
							new BinaryScalarVector(binaryVector, scalarVector)
						},
						cardinality
					)

				}}.collect

				info.foreach{ case (clusterID, mean, cardinality) =>
				{
					centersUpdated(clusterID) = mean
					clustersCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = centersUpdated.forall{ case (clusterID, uptMod) => metric.d(centers(clusterID), uptMod) <= epsilon }
				
				centersUpdated.foreach{ case (clusterID, mod) => centers(clusterID) = mod }	
			}
			else
			{
				println("Results will have no sense or cost O(n²) for the moment with another distance than Euclidean, but we're working on it")
			}
			cpt += 1
		}
		new KPrototypesModel(centers, metric)
	}
}


object KPrototypes extends DataSetsTypes[Long, BinaryScalarVector]
{
	def run(@(transient @param) sc: SparkContext, data: RDD[BinaryScalarVector], k: Int, epsilon: Double, maxIter: Int, metric: MixtDistance): KPrototypesModel =
	{
		val kPrototypes = new KPrototypes(sc, data, k, epsilon, maxIter, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}