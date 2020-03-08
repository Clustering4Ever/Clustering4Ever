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
import breeze.linalg.{DenseMatrix, DenseVector}
import _root_.scala.collection.mutable

sealed trait RPTree extends Serializable {
  /**
   *
   */
  val nbHyperplane: Int
  /**
   *
   */
  val leafSize: Int
}

final case class RPNode(val hyperplane: DenseVector[Double], val offset: Double, val left: RPTree, val right: RPTree, val leafSize: Int) extends RPTree {
    final val nbHyperplane = hyperplane.length
}

final case class RPLeaf(val indices: mutable.ArrayBuffer[Int], val leafSize: Int) extends RPTree {
    final val nbHyperplane = 0
}
/**
 *
 */
object RPTree {
  /**
    * @param rp a random projection tree
    * @return the height of the RP tree
    */
  final def height(rp: RPTree): Int = {
    @annotation.tailrec
    def goHeight(t: List[RPTree], size: Int): Int = {
      t match {
        case RPLeaf(_, _) :: tail => goHeight(tail, size + 1)
        case RPNode(_, _, left, right, _) :: tail => goHeight(List(left, right) ::: tail, size + 1)
        case Nil => size
      }
    }

    goHeight(List(rp), 0)
  }
  /**
    *
    * @param rp a random projection tree
    * @return the number of nodes in the RP tree
    */
  final def nodes(rp: RPTree): Int = {
    @annotation.tailrec
    def go(rp: List[RPTree], size: Int): Int = {
      rp match {
          case Nil => size
          case RPLeaf(_, _) :: tail => go(tail, size + 1)
          case RPNode(_, _, left, right, _) :: tail => go(List(left, right) ::: tail, size + 1)
      }
    }
    go(List(rp), 0)
  }
  /**
    *
    * @param rp a random projection tree
    * @return the number of leaves in the RP tree
    */
  final def leaves(rp: RPTree): Int = {
    @annotation.tailrec
    def go(rp: List[RPTree], size: Int): Int = {
      rp match {
          case Nil => size
          case RPLeaf(_, _) :: tail => go(tail, size + 1)
          case RPNode(_, _, left, right, _) :: tail => go(List(left, right) ::: tail, size)
      }
    }
    go(List(rp), 0)
  }
  /**
    * Construct a random euclidean projection tree based on ``data`` with leaves
    * of size at most ``leaf_size`` with a angular distance.
    *
    * @param data The original data to be split
    * @param indices
    * @param leafSize The maximum size of any leaf node in the tree. Any node in the three with more than leafSize will be split further to create child nodes.
    * @return A random projection tree node which links to its child nodes. This provides the full tree below the returned node.
    */
  final def makeEuclideanTree(data: DenseMatrix[Double], indices: mutable.ArrayBuffer[Int], leafSize: Int): RPTree = {
    if (indices.length > leafSize) {
        val erp = EuclideanRPSplit(data, indices)

        val left = makeEuclideanTree(data, erp.leftIndices, leafSize)
        val right = makeEuclideanTree(data, erp.rightIndices, leafSize)

        RPNode(erp.hyperplane, erp.offset, left, right, leafSize)
    }
    else {
        while (indices.length < leafSize) indices += -1
        RPLeaf(indices, leafSize)
    }
  }
  /**
    * Construct a random angular projection tree based on ``data`` with leaves
    * of size at most ``leaf_size`` with a angular distance.
    *
    * @param data The original data to be split
    * @param indices
    * @param leafSize The maximum size of any leaf node in the tree. Any node in the three with more than leafSize will be split further to create child nodes.
    * @return A random projection tree node which links to its child nodes. This provides the full tree below the returned node.
    */
  final def makeAngularTree(data: DenseMatrix[Double], indices: mutable.ArrayBuffer[Int], leafSize: Int): RPTree = {
    if (indices.length > leafSize) {
        val erp = EuclideanRPSplit(data, indices)

        val left = makeAngularTree(data, erp.leftIndices, leafSize)
        val right = makeAngularTree(data, erp.rightIndices, leafSize)

        RPNode(erp.hyperplane, erp.offset, left, right, leafSize)
    }
    else {
        while (indices.length < leafSize) indices += -1
        RPLeaf(indices, leafSize)
    }
  }
  /**
    * Construct a random projection tree based on ``data`` with leaves
    * of size at most ``leaf_size``.
    *
    *
    * @param data The original data to be split
    * @indices
    * @leafSize The maximum size of any leaf node in the tree. Any node in the three with more than leafSize will be split further to create child nodes.
    * @angular Whether to use cosine/angular distance to create splits in the tree
    * @return A random projection tree node which links to its child nodes. This provides the full tree below the returned node.
    */
  final def makeTree(data: DenseMatrix[Double], indices: mutable.ArrayBuffer[Int], leafSize: Int, angular: Boolean = false): RPTree = {
    if (angular) {
        makeAngularTree(data, indices, leafSize)
    }
    else {
        makeEuclideanTree(data, indices, leafSize)
    }
  }

  def print(rp: RPTree): Unit = {
    def go(t: RPTree): Unit = {
        t match {
            case RPLeaf(v, _) => {
                println("leaf : ")
                v.foreach(println)
                println("")
            }
            case RPNode(h, _, left, right, _) => {
                go(left)
                println("node : ")
                h.foreach(println)
                println("")
                go(right)
            }
        }
    }
    go(rp)
  }
}
