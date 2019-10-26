package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.apache.spark.SparkContext
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.clustering.kcenters.scala.{KCommons, KCentersModelCommons}
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.rdd.ClusteringAlgorithmDistributed
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.kcenters.scala.KPPInitializer
/**
 *
 */
object KPPInitializerSpark {

	def selectKCentersWithKPPInitializer[V <: GVector[V] : ClassTag, D <: Distance[V]](preCentersOverSampled: mutable.ArrayBuffer[V], k: Int, metric: D): immutable.HashMap[Int, V] = {
		val formatedPreFinalCenters = preCentersOverSampled.zipWithIndex.map{ case (v, id) => EasyClusterizable(id, v) }
		val kInitCenters = KPPInitializer.kppInit(formatedPreFinalCenters, metric, k)
		kInitCenters

	}

	/**
	 * @param vectorizedDataset
	 * @param k seaked cluster number
	 * @param metric
	 * @param finalSelection methods to select final K-Centers from oversampling 
	 * @param l oversampling factor
	 * @param numIter number of iteration to generate over samples
	 * @param centerNumberPerPartition number of centers generated for each partition (k * l)
	 */
	def parallelKmPPInitialization[V <: GVector[V] : ClassTag, D <: Distance[V]](vectorizedDataset: RDD[V], k: Int, metric: D, finalSelection: (mutable.ArrayBuffer[V], Int, D) => immutable.HashMap[Int, V], l: Int = 10, numIter: Int = 0, centerNumberPerPartition: Int = 2): immutable.HashMap[Int, V] = {
			val centers = mutable.ArrayBuffer(vectorizedDataset.sample(false, 0.001, 8L).collect.head)
			val firstCenter = centers.head
			def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
			def firstAcc(agg: Double, v: V): Double = {
				val toPow2 = metric.d(v, firstCenter)
				agg + toPow2 * toPow2				
			}

			val phi = vectorizedDataset.aggregate(0D)(firstAcc, _ + _)

			// Each partition will generate a new prototypes
			val vectorizedDatasetRepartitionned = vectorizedDataset.repartition(k * l)

			val logPhiBound = math.log(phi).toInt
			val iterMax = if (numIter <= 0) logPhiBound else numIter
			@annotation.tailrec
			def go(phi: Double, centers: mutable.ArrayBuffer[V], i: Int): mutable.ArrayBuffer[V] = {
				if (i < iterMax) {
					
					val preprocessed = vectorizedDatasetRepartitionned.map{ v =>
						val toPow2 = metric.d(v, obtainNearestCenter(v, centers))
						(v, toPow2 * toPow2)
					}
					
					val newPhi = preprocessed.aggregate(0D)((agg, e) => agg + e._2, _ + _)

					val preCenters = preprocessed.mapPartitions{ it =>

						val probabilities = it.map{ case (v, toPow2) =>
							val toPow2 = metric.d(v, obtainNearestCenter(v, centers))
							(v, (l * toPow2) / newPhi)
						}

						val preCenters = (0 until centerNumberPerPartition).map{ _ =>
							Stats.obtainMedianFollowingWeightedDistribution(Random.shuffle(probabilities.toVector))
						}

						preCenters.toIterator

					}.collect

					centers ++= preCenters
					
					go(newPhi, centers, i + 1)
				}
				else centers
			}

			val preFinalCenters = go(phi, centers, 0)

			finalSelection(preFinalCenters, k, metric)
	}

	/**
	 * @param vectorizedDataset
	 * @param k seaked cluster number
	 * @param metric
	 * @param l oversampling factor
	 * @param numIter number of iteration to generate over samples
	 * @param centerNumberPerPartition number of centers generated for each partition (k * l)
	 */
	def pseudoEasyParallelKmPPInitialization[V <: GVector[V] : ClassTag, D <: Distance[V]](vectorizedDataset: RDD[V], k: Int, metric: D, l: Int = 10, numIter: Int = 0, centerNumberPerPartition: Int = 2): immutable.HashMap[Int, V] = {
		parallelKmPPInitialization(vectorizedDataset, k, metric, selectKCentersWithKPPInitializer[V, D] _, l, numIter, centerNumberPerPartition)
	}

}