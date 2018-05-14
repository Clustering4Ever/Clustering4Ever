package clustering4ever.spark.clustering.clusterwise

import _root_.scala.collection.mutable.ArrayBuffer
import breeze.linalg.DenseMatrix

trait ClusterwiseTypes
{
	type ClassID = Int
	type ID = Int
	type Xvector = Array[Double]
	type Yvector = Array[Double]
	type IDXYtest = Array[(Int, (Xvector, Yvector))]
	type IDXtest = Array[(Long, Xvector)]
	type DSPerClass = Array[(ID, (Xvector, Yvector, ClassID))]
	type ClassedDS = Array[(Int, DSPerClass)]
	type IDXDS = Array[ArrayBuffer[(Int, Xvector)]]
	type YDS = Array[ArrayBuffer[Yvector]]
	type RegPerClass = (Double, DenseMatrix[Double], Array[Double], Array[(Int, Array[Double])])
	type ClassedDSperGrp = Array[(Int, Array[(Int, Int, Array[(ClassID, Int, Xvector, Yvector)])])]
}