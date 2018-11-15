package org.clustering4ever.spark.clustering.clusterwise

import scala.collection.{mutable, immutable}
import breeze.linalg.DenseMatrix

trait ClusterwiseTypes[S <: Seq[Double]] {
	type ClassID = Int
	type ID = Int
	type Xvector = S
	type Yvector = S
	type IDXYtest = Seq[(Int, (Xvector, Yvector))]
	type IDXtest = Seq[(Long, Xvector)]
	type DSPerClass = Array[(ID, (Xvector, Yvector, ClassID))]
	type ClassedDS = Array[(Int, DSPerClass)]
	type IDXDS = Array[mutable.ArrayBuffer[(Int, Xvector)]]
	type YDS = Array[mutable.ArrayBuffer[Yvector]]
	type RegPerClass = (Double, DenseMatrix[Double], Array[Double], immutable.IndexedSeq[(Int, Array[Double])])
	type ClassedDSperGrp = Array[(Int, Array[(Int, Int, Array[(ClassID, Int, Xvector, Yvector)])])]
}