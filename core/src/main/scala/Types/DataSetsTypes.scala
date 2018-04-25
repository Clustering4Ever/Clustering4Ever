package clustering4ever.clustering.datasetstype

import clustering4ever.clustering.ClusteringCommons

trait DataSetsTypes[IDNature, InputVector] extends ClusteringCommons
{
	type ID = IDNature
	type Vector = InputVector
}