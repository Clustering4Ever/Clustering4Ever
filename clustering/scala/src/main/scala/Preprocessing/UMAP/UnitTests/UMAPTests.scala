package org.clustering4ever.scala.umap.tests
/**
 * @author Beugnet Vincent
 * @author Hurvois Guillaume
 * @author Ladjal Adlane
 * @author Merien Grégoire
 * @author Serfas Florent
 * @author Beck Gaël
 * @author Forest Florent
 */
import breeze.linalg.DenseMatrix
import org.clustering4ever.scala.umap.Utils

import scala.util.Random

object SmoothKNNandMemberShipTests {
    def casualTest = {
        val rand = new Random
        val dist = DenseMatrix.zeros[Double](15, 15).map(_ => rand.nextDouble * 20d - 10d)
        val ind = DenseMatrix.zeros[Double](15, 15).map(_ => rand.nextInt(15))

        val a = org.clustering4ever.scala.umap.UMAP.smoothKNNDist(dist, 15)
        val rhos = a._1
        val sigmas = a._2


        val (rows, cols, vals) = org.clustering4ever.scala.umap.UMAP.membershipStrengths(ind, dist, rhos, sigmas)
        println(Utils.makeMatrix(rows, cols, vals, 15, 3))
        true
    }
}

object FindABParamsTests {
    def casualTest = {
        val ab = org.clustering4ever.scala.umap.UMAP.findABParams(4.5, 3.8)
        println("a = " + ab._1 + ", b = " + ab._2)

    }

    def nullSpreadTest = {
        val ab = org.clustering4ever.scala.umap.UMAP.findABParams(0d, 3.8)
        println("a = " + ab._1 + ", b = " + ab._2)

    }

    def nullMinDistTest = {
        val ab = org.clustering4ever.scala.umap.UMAP.findABParams(4.5, 0d)
        println("a = " + ab._1 + ", b = " + ab._2)
    }

    def nullTest = {
        val ab = org.clustering4ever.scala.umap.UMAP.findABParams(0d, 0d)
        println("a = " + ab._1 + ", b = " + ab._2)
    }
}
