package clustering4ever.spark.clustering.clusterwise

import scala.collection.{mutable, immutable}
import breeze.linalg.DenseMatrix

trait ClusterwiseTypes
{
	type ClassID = Int
	type ID = Int
	type Xvector = Seq[Double]
	type Yvector = Seq[Double]
	type IDXYtest = Seq[(Int, (Xvector, Yvector))]
	type IDXtest = Seq[(Long, Xvector)]
	type DSPerClass = Array[(ID, (Xvector, Yvector, ClassID))]
	type ClassedDS = Array[(Int, DSPerClass)]
	type IDXDS = Array[mutable.ArrayBuffer[(Int, Xvector)]]
	type YDS = Array[mutable.ArrayBuffer[Yvector]]
	type RegPerClass = (Double, DenseMatrix[Double], Array[Double], immutable.Vector[(Int, immutable.Vector[Double])])
	type ClassedDSperGrp = Array[(Int, Array[(Int, Int, Array[(ClassID, Int, Xvector, Yvector)])])]
}