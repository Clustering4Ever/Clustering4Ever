package clustering4ever.clustering.datasetstype

trait DataSetsTypes
{
	type ID
	type NaturesValue
	type ClusterID = Int
	type Vector = Array[NaturesValue]
	type OriginalVector = Array[NaturesValue]
	type Mod = Array[NaturesValue]
	type ClusterizedData = Array[(ClusterID, (ID, Vector))]

}

trait RealDatasets extends DataSetsTypes
{
	type NaturesValue = Double
}

trait BinaryDatasets extends DataSetsTypes
{
	type NaturesValue = Int
}

trait ScalaDatasetsTypes extends DataSetsTypes
{
	type ID = Int
}

trait SparkDatasetsTypes extends DataSetsTypes
{
	type ID = Long
}

trait RealScalaDatasets extends RealDatasets with ScalaDatasetsTypes

trait BinaryScalaDatasets extends BinaryDatasets with ScalaDatasetsTypes

trait RealSparkDatasets extends RealDatasets with SparkDatasetsTypes

trait BinarySparkDatasets extends BinaryDatasets with SparkDatasetsTypes

trait MeanShiftTypes
{
	// Mean Shift Gradient Ascent
	type IsOriginalPoint = Boolean
	type HasConverged = Boolean
	type IndexPartition = Int
}