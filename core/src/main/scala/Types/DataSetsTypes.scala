package clustering4ever.clustering.datasetstype

import clustering4ever.clustering.ClusteringCommons

trait DataSetsTypes[IDNature] extends ClusteringCommons
{
	type ID = IDNature
}