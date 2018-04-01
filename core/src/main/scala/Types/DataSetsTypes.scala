package clustering4ever.clustering.datasetstype

trait DataSetsTypes[IDNature, NaturesValue]
{
	type ID = IDNature
	type ClusterID = Int
	type Vector = Array[NaturesValue]
	type OriginalVector = Array[NaturesValue]
	type Mod = Array[NaturesValue]
}

trait ClusteringTypes[IDNature, NaturesValue, ClusterizedNature] extends DataSetsTypes[IDNature, NaturesValue]
{
	type ClusterizedData = ClusterizedNature
}