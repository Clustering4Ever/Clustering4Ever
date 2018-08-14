package clustering4ever.spark.clustering.clusterwise

import scala.collection.{mutable, immutable}
import breeze.linalg.DenseMatrix

trait ClusterwiseTypes
{
	type ClassID = Int
	type ID = Int
	type Xvector = immutable.Seq[Double]
	type Yvector = immutable.Seq[Double]
	type IDXYtest = immutable.Seq[(Int, (Xvector, Yvector))]
	type IDXtest = immutable.Seq[(Long, Xvector)]
	type DSPerClass = immutable.Vector[(ID, (Xvector, Yvector, ClassID))]
	type ClassedDS = immutable.Vector[(Int, DSPerClass)]
	type IDXDS = immutable.Vector[mutable.ArrayBuffer[(Int, Xvector)]]
	type YDS = immutable.Vector[mutable.ArrayBuffer[Yvector]]
	type RegPerClass = (Double, DenseMatrix[Double], Array[Double], immutable.Vector[(Int, immutable.Vector[Double])])
	type ClassedDSperGrp = immutable.Vector[(Int, Array[(Int, Int, immutable.Vector[(ClassID, Int, Xvector, Yvector)])])]
}