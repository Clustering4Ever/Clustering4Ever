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
import java.io.File
import breeze.linalg._
import scala.io.Source


object MainTest {
    def main(args: Array[String]): Unit = {
        val path = "adlane-MAC"
        val namefile = path match {
            case "adlane-MAC" => "/Users/adlaneladjal/Desktop/UMAP-tests/breast-cancer/breast-cancer.data"
            case "adlane-PC" => "/home/adlane/OneDrive/Etudes/semestre7/CGP/cgp-umap/test/data/digits.data"
            case "florent" => "/florent/path/cgp-umap/test/data/digits.data"
            case "guillaume" => "/guillaume/path/cgp-umap/test/data/digits.data"
            case "gregoire" => "/gregoire/path/cgp-umap/test/data/digits.data"
            case "vincent" => "/vincent/path/cgp-umap/test/data/digits.data"
        }

        val nbRows = Source.fromFile(namefile).getLines.size
        val data = Source.fromFile(namefile).getLines.toArray.flatMap(_.split(",")).map(_.toDouble)

        val M = new DenseMatrix(data.length/nbRows, data, offset = 0).t

        val umap = new UMAP(init = "random")

        val t0 = System.nanoTime()
        val emb = umap.fit(M)
        val t1 = System.nanoTime()


        println(emb)
        println("Temps d'execution Apprentissage : " + (t1 - t0) + "ns")
        csvwrite(new File("/Users/adlaneladjal/Desktop/UMAP-tests/breast-cancer/UMAPRandom.txt"), emb, separator = ' ')


        /*val adjency = DenseMatrix((0, 1, 1, 1, 0), (1, 0, 1, 0, 0), (1, 1, 0, 0, 0), (1, 0, 0, 0, 1), (0, 0, 0, 1, 0))

        val marking: mutable.ArrayBuffer[Boolean] = mutable.ArrayBuffer.fill(adjency.rows)(false)

        def explore(graph: DenseMatrix[Int], s: Int, marks: mutable.ArrayBuffer[Boolean]): Unit = {
            marks(s) = true
            val sline = graph(s, ::).t
            val child = sline.findAll(i => i != 0)
            if (child.exists(i => ! marks(i))) {
                child.foreach(i => if (!marks(i)) explore(graph, i, marks))
            }
        }
        explore(adjency, 0, marking)
        val a = eig(convert(adjency, Double))
        println(a)
        println(marking)*/

        /*def randomMatrix(implicit random: Random): DenseMatrix[Double] =
            new DenseMatrix[Double](60, 3).map(_ => random.nextDouble * 20d - 10d)

        val initPoints = randomMatrix(new Random) // pour l'exemple
        val arrLines = new mutable.ArrayBuffer[Seq[Double]]

        for (i <- 0 until 60) {
            val vec = initPoints(i, ::).t
            val vecArr = vec.toArray
            val vecSeq = vecArr.toSeq
            arrLines += vecSeq
        }


        val q = KDTree.fromSeq(arrLines)(DimensionalOrdering.dimensionalOrderingForSeq[Seq[Double], Double](3))

        println(q.findNearest(Seq(2.5, -7.8, 0.6), 2))*/

    }
}