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
import breeze.linalg._
import _root_.scala.collection.mutable.ArrayBuffer

/**
  * A FlatTree is a flat form for an RPTree. It is made to improve
  * searching time through the tree.
  *
  * @param rp       The RPTree to transform.
  * @param leafSize The size of the flat tree's leaves.
  */
case class FlatTree(rp: RPTree, leafSize: Int) {

    private val nodes = RPTree.nodes(rp)
    private val leaves = RPTree.leaves(rp)

    val hyperplane: ArrayBuffer[DenseVector[Double]] = new ArrayBuffer[DenseVector[Double]](nodes)
    val offsets: ArrayBuffer[Double] = new ArrayBuffer[Double](nodes)
    val children: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)](nodes)
    var indices: DenseMatrix[Int] = new DenseMatrix[Int](leaves, leafSize)

    init
    flatten(rp)

    private def init = {
        val zun = 0 until nodes
        zun.foreach(_ => {
            hyperplane += DenseVector.zeros[Double](rp.nbHyperplane)
            offsets += 0
            children += Tuple2(-1, -1)
        })
        indices = -1 * DenseMatrix.ones[Int](leaves, leafSize)
    }


    /**
      * Recursively fills the fields of the FlatTree while browsing through the RPTree.
      *
      * @param tree    A list containing the RPTrees to transform and the number assigned to their roots.
      * @param nodeNum The number that will be assigned to the next node.
      * @param leafNum The number that will be assigned to the next leaf.
      */
    private def flatten(rp: RPTree) : Unit = {
        @annotation.tailrec
        def recursiveFlatten(tree: List[RPTree Tuple2 Int], nodeNum: Int, leafNum: Int) : (Int, Int) = {
            tree match {
                case (RPLeaf(treeIndices, _), node) :: tail =>
                    children(node) = (-leafNum, -1)
                    indices(leafNum, ::) := DenseVector(treeIndices: _*).t
                    recursiveFlatten(tail, nodeNum, leafNum + 1)
                case (RPNode(treeHyperplane, treeOffsets, leftChild, rightChild, _), node) :: tail =>
                    hyperplane(node) += treeHyperplane
                    offsets(node) = treeOffsets
                    children(node) = (nodeNum, nodeNum + 1)
                    recursiveFlatten(List((leftChild, nodeNum), (rightChild, nodeNum + 1)) ++ tail, nodeNum + 2, leafNum)
                case Nil => (nodeNum, leafNum)
            }
        }
        recursiveFlatten(List((rp, 0)), 1, 0)
    }


    /**
      * Given a point, returns the indices of the leaf that could have been its father in the tree.
      *
      * @param point    The point which side we are looking for.
      * @param state    The initialization array for random.
      * @param node     The number of the actual node. Default is 0 for the tree's root.
      * @return A vector containing the indices of the father leaf.
      */
    @annotation.tailrec
    final def searchFlatTree(point: DenseVector[Double], state : Array[Long], node : Int = 0) : DenseVector[Int] = {
        if (children(node)._2 == -1)
            indices(children(node)._1, ::).t
        else if (selectSide(node, point, state))
            searchFlatTree(point, state, children(node)._1)
        else
            searchFlatTree(point, state, children(node)._2)
    }


    /**
      * Given a point, returns which side of the tree the point should be in.
      *
      * @param node     The number of the node that is the root of the tree that we are looking into.
      * @param point    The point which side in the tree we are looking for.
      * @param state    The initialization array for random.
      * @return A boolean representing the side (true for left, false for right).
      */
    def selectSide(node: Int, point: DenseVector[Double], state : Array[Long]) : Boolean = {
        val margin = offsets(node) + sum(hyperplane(node) *:* point)
        // true if margin < 0, false if margin > 0 and random if margin = 0
        margin < 0 || ((margin == 0) && (Utils.tauRandInt(state)%2 != 0))
    }
}
