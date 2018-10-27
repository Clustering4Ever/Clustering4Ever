package clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import scala.collection.mutable
import breeze.stats.mean
import clustering4ever.clustering.ClusteringAlgorithms
/**
 *
 */
class ThSumFibers(val k1: Int, val k2: Int, val tensor: mutable.ListBuffer[DenseMatrix[Double]]) extends ClusteringAlgorithms {
	/**
	 *
	 */
  	def run() = {

		val m = tensor.length
		val n1 = tensor.head.rows
		val n2 = tensor.head.cols

		//Build a matrix D such that each element is the norm of trajectories
		@annotation.tailrec
        def indiceFiber(tensor: mutable.ListBuffer[DenseMatrix[Double]], v: DenseVector[Double], d: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
			if( i < tensor.head.rows && j < tensor.head.cols && k < tensor.length ) {
				v(k) = tensor(k)(i, j)
				indiceFiber(tensor, v, d, i, j, k + 1)
			}
			else if( k == tensor.length && i < tensor(0).rows && j < tensor(0).cols ) {
				d(i, j) = norm(v)
				indiceFiber(tensor, v, d, i + 1, j , 0)
			}

			else if( j < tensor(0).cols - 1 ) {
				indiceFiber(tensor, v, d, 0, j + 1, 0)
			}
			else {
				d
			}
		}

		val traj = DenseVector.zeros[Double](m)
		val matrixNorm = indiceFiber(tensor, traj, DenseMatrix.zeros[Double](n1, n2), 0, 0, 0)

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

    def train(k1: Int, k2: Int, tensor: mutable.ListBuffer[DenseMatrix[Double]]) = (new ThSumFibers(k1, k2, tensor)).run()

}