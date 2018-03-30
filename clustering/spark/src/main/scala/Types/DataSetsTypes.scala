package clustering4ever.clustering.datasetstype

import org.apache.spark.rdd.RDD

trait RDDDataSetsTypes
{
	type ID = Long
	type NaturesValue
	type ClusterID = Int
	type Vector = Array[NaturesValue]
	type OriginalVector = Array[NaturesValue]
	type Mod = Array[NaturesValue]
	type ClusterizedRDD = RDD[(ClusterID, (ID, Vector))]
}
