package org.clustering4ever.clustering.tensor

/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import breeze.linalg._
import org.clustering4ever.clusteringtraits.ClusteringAlgorithm
import org.clustering4ever.roottraits.TensorBiclustering

import scala.collection.mutable
/**
 *
 */
class ThIndFibers(val k1: Int, val k2: Int) extends ClusteringAlgorithm {

	final val algorithmID = TensorBiclustering
	/**
	 *
	 */
  	def fit(data: mutable.ArrayBuffer[DenseMatrix[Double]]) = {

	    val m = data.length
	    val n1 = data.head.rows
	    val n2 = data.head.cols

        //This function build a n1 by n2 matrix D such that each element of D is the euclidean norm of the corresponding trajectory
	    @annotation.tailrec
		def indiceFiber(t: mutable.ArrayBuffer[DenseMatrix[Double]], v: DenseVector[Double], d: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
			if( i < t.head.rows && j < t.head.cols && k < t.length ) {
				v(k) = t(k)(i, j)
				indiceFiber(t,v, d, i, j, k + 1)
			}
			else if( k == t.length && i < t.head.rows && j < t.head.cols ) {
				d(i, j) = norm(v)
				indiceFiber(t, v, d, i + 1, j , 0)
			}
			else if( j < t.head.cols - 1 ) {
				indiceFiber(t, v, d, 0, j + 1, 0)
			}
			else {
				d
			}
	    }

	    val traj = DenseVector.zeros[Double](m)
	    val matrixnorm = indiceFiber(data, traj, DenseMatrix.zeros[Double](n1, n2), 0, 0, 0)

        //This function build a binary matrix from the matrix D such that the top k1*k2 elements return to 1 and zero the remainsaccording to  the cardinality of the index sets k1 and k2
	    @annotation.tailrec
	    def hightvalue(normMatrix: DenseMatrix[Double], binMatrix: DenseMatrix[Int], nbr: Int, i: Int, j: Int): DenseMatrix[Int] = {

			val maxi = max(normMatrix)
	        if ( maxi == normMatrix(i, j) && nbr > 0 ) {
	          binMatrix(i, j) = 1
	          normMatrix(i, j) = 0D
	          hightvalue(normMatrix, binMatrix, nbr - 1, 0, 0)
	        }
	        else if( i < normMatrix.rows - 1 ) {
	          hightvalue(normMatrix, binMatrix, nbr, i+1, j)
	        }
	        else if( j < normMatrix.cols - 1 ) {
	          hightvalue(normMatrix, binMatrix, nbr, 0, j+1)
	        }
	        else {
	          binMatrix
	        }

	    }

	    val matrixtrajBin = hightvalue(matrixnorm, DenseMatrix.zeros[Int](n1,n2), k1*k2, 0, 0)

	    val sl = sum(matrixtrajBin(*,::))
	    val indiceRow = TensorCommons.obtainTopkIndices[Int](sl, k1)
	    val sc = sum(matrixtrajBin(::,*)).t
	    val indiceColumn= TensorCommons.obtainTopkIndices[Int](sc, k2)
    
		new TensorBiclusteringModel(indiceRow.distinct.sorted, indiceColumn.distinct.sorted)

		Array(indiceRow, indiceColumn)
    }
}
/**
 *
 */
object ThIndFibers {

    def train(k1: Int, k2: Int, data: mutable.ArrayBuffer[DenseMatrix[Double]]) = (new ThIndFibers(k1, k2)).fit(data)

}

