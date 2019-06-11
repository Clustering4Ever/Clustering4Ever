package org.clustering4ever.scala.umap
/**
 * @author Beugnet Vincent
 * @author Hurvois Guillaume
 * @author Ladjal Adlane
 * @author Merien Grégoire
 * @author Serfas Florent
 * @author Beck Gaël
 * @author Forest Florent
 */
import breeze.linalg.{DenseMatrix, max}
import scala.collection.mutable

/**
  * A Forest is a group of FlatTrees. It is used to concatenate a certain amount of tree leaves.
  *
  * @param data         The data we want to transform.
  * @param nbNeighbors  The size of a leaf (ignored if < 10).
  * @param nbTrees      The amount of trees to create in the forest.
  * @param state        The initialization array for random.
  * @param angular      Whether the trees must be angular or not.
  */
case class Forest(data: DenseMatrix[Double], nbNeighbors: Int, nbTrees: Int, state : Array[Long], angular: Boolean = false) {
  val trees = new mutable.ArrayBuffer[FlatTree](0)

  makeForest

  /**
    * Creates a forest of FlatTrees by adding them recursively.
    */
  private def makeForest : Unit = {
    val leafSize = max(10, nbNeighbors)
    val indices = mutable.ArrayBuffer(0 until data.rows: _*)

    @annotation.tailrec
    def addTree(nbTree: Int) : Unit = {
      if (nbTree > 0) {
        val tree = RPTree.makeTree(data, indices, leafSize, angular)
        trees += new FlatTree(tree, leafSize)
        addTree(nbTree - 1)
      }
    }

    addTree(nbTrees)
  }

  /**
    * Creates a matrix of leaves by vertically concatenating the leaf matrix of every tree in the forest.
    *
    * @return A matrix containing the leaf matrix of every tree concatenated vertically.
    */
  def leafArray : DenseMatrix[Int] = {
    trees.size match {
      case 0 => - DenseMatrix.eye[Int](1)
      case _ =>
        @annotation.tailrec
        def concat(array: DenseMatrix[Int], i: Int): DenseMatrix[Int] = {
          if (i >= trees.size)
            array
          else
            concat(DenseMatrix.vertcat(array, trees(i).indices), i + 1)
        }
        concat(trees(0).indices, 1)
    }
  }
}
