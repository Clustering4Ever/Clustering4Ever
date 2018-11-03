package clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import scala.collection.mutable
import breeze.stats.mean
import clustering4ever.clustering.LocalClusteringAlgorithm
/**
 *
 */
class ThSumFibers(val k1: Int, val k2: Int) extends LocalClusteringAlgorithm[mutable.ListBuffer[DenseMatrix[Double]]] {
	/**
	 *
	 */
  	def run(data: mutable.ListBuffer[DenseMatrix[Double]]) = {

		val m = data.length
		val n1 = data.head.rows
		val n2 = data.head.cols

		//Build a matrix D such that each element is the norm of trajectories
		@annotation.tailrec
        def indiceFiber(data: mutable.ListBuffer[DenseMatrix[Double]], v: DenseVector[Double], d: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
			if( i < data.head.rows && j < data.head.cols && k < data.length ) {
				v(k) = data(k)(i, j)
				indiceFiber(data, v, d, i, j, k + 1)
			}
			else if( k == data.length && i < data(0).rows && j < data(0).cols ) {
				d(i, j) = norm(v)
				indiceFiber(data, v, d, i + 1, j , 0)
			}

			else if( j < data(0).cols - 1 ) {
				indiceFiber(data, v, d, 0, j + 1, 0)
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

    def train(k1: Int, k2: Int, data: mutable.ListBuffer[DenseMatrix[Double]]) = (new ThSumFibers(k1, k2)).run(data)

}