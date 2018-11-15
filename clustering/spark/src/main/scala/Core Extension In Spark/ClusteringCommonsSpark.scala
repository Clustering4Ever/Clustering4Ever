package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
 /**
 * The basic trait shared by all distributed clustering algorithms
 */
trait DistributedClusteringAlgorithm[DataType <: RDD[_]] extends ClusteringAlgorithm[DataType]