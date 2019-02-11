package org.clustering4ever.spark.clustering.clusterwise

import scala.collection.{mutable, immutable}
import breeze.linalg.DenseMatrix

trait ClusterwiseTypes[S <: Seq[Double]] {
	final type ClassID = Int
	final type ID = Int
	final type Xvector = S
	final type Yvector = S
	final type IDXYtest = Seq[(Int, (Xvector, Yvector))]
	final type IDXtest = Seq[(Long, Xvector)]
	final type DSPerClass = Array[(ID, (Xvector, Yvector, ClassID))]
	final type ClassedDS = Array[(Int, DSPerClass)]
	final type IDXDS = Array[mutable.ArrayBuffer[(Int, Xvector)]]
	final type YDS = Array[mutable.ArrayBuffer[Yvector]]
	final type RegPerClass = (Double, DenseMatrix[Double], Array[Double], immutable.IndexedSeq[(Int, Array[Double])])
	final type ClassedDSperGrp = Array[(Int, Array[(Int, Int, Array[(ClassID, Int, Xvector, Yvector)])])]
}