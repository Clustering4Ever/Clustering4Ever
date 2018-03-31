
package clustering4ever.spark.clustering.mtm.utils
import java.io._
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.DenseVector
import clustering4ever.spark.clustering.mtm.global.AbstractModel
import clustering4ever.spark.clustering.mtm.global.AbstractPrototype
import clustering4ever.spark.clustering.mtm.global.AbstractTrainer

import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.File
import java.io.FileWriter

object Output extends Serializable {
/*
  def addHeaderToRdd(sparkCtx: SparkContext, lines: RDD[String], header: String): RDD[String] = {
    val headerRDD = sparkCtx.parallelize(List((-1L, header)))     // index the header with -1, so that the sort will put it on top.
    val pairRDD = lines.zipWithIndex()
    val pairRDD2 = pairRDD.map(t => (t._2, t._1))
    val allRDD = pairRDD2.union(headerRDD)
    val allSortedRDD = allRDD.sortByKey()
    allSortedRDD.values
  }
*/
  def saveStr(savingPath: String, value: String, fileName: String = "") =
  {
    val finalPath = savingPath + fileName
    val fw = new FileWriter(finalPath, true)
    fw.write(value + "\n")
    fw.close    
  }

  def write(outputDir: String, datas: RDD[DenseVector], model: AbstractModel, nbRowSOM:Int, nbColSOM: Int): String = {
      val now = Calendar.getInstance().getTime()
      val format = new SimpleDateFormat("yyyyMMddHHmmss")
      val time = format.format(now)
      val dim = datas.take(1)(0).toArray.length
      val datasWithIndex = datas.zipWithIndex.map(t => (t._2, t._1))

      val path: String = outputDir + "/EXP-" + time
      val dir: File = new File(path)
      dir.mkdir();

    
      val mapMin = Array.fill[Byte](dim)(0).mkString(",")
      var header = "# mapDim=2 mapSize={"+ nbRowSOM +"," + nbColSOM + "}"
      header += " pointDim=" + dim + " pointRealDim=" + dim + " mapMin={" + mapMin + "}"
    
      val prototypes = model.prototypes.map(d => (d.id, d._point)).sortBy(_._1).map(_._2)
      println("Write Prototypes...")
      val protosString = prototypes.map(d => d.toArray.mkString(",")).mkString("\n")
     
      //val protosResult = addHeaderToRdd(sc, protosString, header)

      // Utiliser fileWriter
     saveStr(path, header + "\n" + protosString, "maps")

     // protosResult.coalesce(1).saveAsTextFile(path.toString+ "/maps-")

      val sumAffectedDatas = datas.map(d => (model.findClosestPrototype(d).id, 1))
        .reduceByKey{case (sum1, sum2) => sum1+sum2}
        .collectAsMap() 
    
      // fill in all the prototypes that have 0 observations
      val card = (0 to prototypes.length - 1).map(d => {
        if (sumAffectedDatas.contains(d)) {
          sumAffectedDatas(d) + ""
        } else {
          "0"
        }
      })
    
      println("Write Cardinalities...")
      var cardHeader = "# mapDim=2 mapSize={"+ nbRowSOM +"," + nbColSOM + "}" 
      cardHeader +=  "pointDim=1 pointRealDim=0 mapMin={0} mapMax={0}"
      //val cardRdd = sc.parallelize(card)
      val cardStr = card.mkString("\n")
      //val cardResult = addHeaderToRdd(sc, cardRdd, cardHeader)
      //cardResult.coalesce(1).saveAsTextFile(path + "/cards")
      saveStr(path, cardHeader + "\n" + cardStr, "cards")
      
    
      val affHeader = "# mapDim=1 mapSize={" + datas.count() + "} pointDim=1 pointRealDim=0 mapMin={0} mapMax={0}"
      val aff = datasWithIndex.map(d => (d._1, model.findClosestPrototype(d._2).id + "")).sortByKey().values.collect.mkString("\n")
      //val affResult = addHeaderToRdd(sc, aff, affHeader)
      println("Write Affiliate...")
      //affResult.coalesce(1).saveAsTextFile(path + "/affs")
      saveStr(path, affHeader + "\n" + aff, "affs")
    
      println("Write Maps...")
      //val maps = sc.parallelize(prototypes.zip(card)).map(d => d._1.toArray.mkString(",") + "," + d._2)
      val maps = prototypes.zip(card).map(d => d._1.toArray.mkString(",") + "," + d._2).mkString("\n")
      saveStr(path, maps, "mapscard")
      println("Write successfully...")
      time
  }
}

