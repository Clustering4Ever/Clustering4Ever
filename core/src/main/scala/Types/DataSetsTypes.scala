package clustering4ever.clustering.datasetstype

trait DataSetsTypes[IDNature, InputVector]
{
	type ID = IDNature
	type ClusterID = Int
	type Vector = InputVector
	//type Vector = Array[NaturesValue]
	//type OriginalVector = Array[NaturesValue]
	//type Mod = Array[NaturesValue]
}