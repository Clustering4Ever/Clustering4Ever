package org.clustering4ever.clustering.kfamily.clusterwise

import breeze.linalg.DenseMatrix

import scala.collection.mutable

trait ClusterwiseTypes {
	final type ClassID = Int
	final type ID = Int
	final type Xvector = Array[Double]
	final type Yvector = Array[Double]
	final type IDXYtest = Seq[(Int, (Xvector, Yvector))]
	final type IDXtest = Seq[(Long, Xvector)]
	final type DSPerClass = Array[(ID, (Xvector, Yvector, ClassID))]
	final type ClassedDS = Array[(Int, DSPerClass)]
	final type IDXDS = Array[mutable.ArrayBuffer[(Int, Xvector)]]
	final type YDS = Array[mutable.ArrayBuffer[Yvector]]
	final type RegPerClass = (Double, DenseMatrix[Double], Array[Double], Array[(Int, Array[Double])])
	final type ClassedDSperGrp = Array[(Int, Array[(Int, Int, Array[(ClassID, Int, Xvector, Yvector)])])]
}