package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import scala.collection.mutable
import breeze.stats.mean
import org.clustering4ever.clustering.LocalClusteringAlgorithm
/**
 *
 */
class ThSumFibers(val k1: Int, val k2: Int) extends LocalClusteringAlgorithm[mutable.ArrayBuffer[DenseMatrix[Double]]] {
	/**
	 *
	 */
  	def run(data: mutable.ArrayBuffer[DenseMatrix[Double]])(implicit workingVector: Int = 0) = {

		val m = data.length
		val n1 = data.head.rows
		val n2 = data.head.cols

		//Build a matrix D such that each element is the norm of trajectories
		@annotation.tailrec
        def indiceFiber(t: mutable.ArrayBuffer[DenseMatrix[Double]], v: DenseVector[Double], d: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
			if( i < t.head.rows && j < t.head.cols && k < t.length ) {
				v(k) = t(k)(i, j)
				indiceFiber(t, v, d, i, j, k + 1)
			}
			else if( k == t.length && i < t(0).rows && j < t(0).cols ) {
				d(i, j) = norm(v)
				indiceFiber(t, v, d, i + 1, j , 0)
			}

			else if( j < t(0).cols - 1 ) {
				indiceFiber(t, v, d, 0, j + 1, 0)
			}
			else {
				d
			}
		}

		val traj = DenseVector.zeros[Double](m)
		val matrixNorm = indiceFiber(data, traj, DenseMatrix.zeros[Double](n1, n2), 0, 0, 0)

		val sl = sum(matrixNorm(*,::))
		val indiceRow = TensorCommons.obtainTopkIndices[Double](sl,k1)
		val sc = sum(matrixNorm(::,*)).t  //we take the transpose to have a column vector
		val indiceColumn = TensorCommons.obtainTopkIndices[Double](sc,k2)

		new TensorBiclusteringModel(indiceRow.distinct.sorted, indiceColumn.distinct.sorted)

	  }
}
/**
 *
 */
object ThSumFibers {

    def train(k1: Int, k2: Int, data: mutable.ArrayBuffer[DenseMatrix[Double]])(implicit workingVector: Int = 0) = (new ThSumFibers(k1, k2)).run(data)(workingVector)

}