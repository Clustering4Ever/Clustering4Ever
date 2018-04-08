package clustering4ever.clustering.datasetstype

trait DataSetsTypes[IDNature, InputVector]
{
	type ID = IDNature
	type ClusterID = Int
	type Vector = InputVector
}