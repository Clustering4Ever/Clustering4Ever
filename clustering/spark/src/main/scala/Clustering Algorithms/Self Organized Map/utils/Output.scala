
package clustering4ever.spark.clustering.mtm.utils
import java.io._
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.DenseVector
import clustering4ever.spark.clustering.mtm.global.AbstractModel
import clustering4ever.spark.clustering.mtm.global.AbstractPrototype
import clustering4ever.spark.clustering.mtm.global.AbstractTrainer
import scala.sys.process._

import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.File
import java.io.FileWriter

object Output extends Serializable
{

  def saveStr(savingPath: String, value: String, fileName: String = "") =
  {
    s"mkdir -p ${savingPath}".!
    val finalPath = savingPath + fileName
    val fw = new FileWriter(finalPath, true)
    fw.write(value + "\n")
    fw.close    
  }

  def write(outputDir: String, datas: RDD[DenseVector], model: AbstractModel, nbRowSOM:Int, nbColSOM: Int): String =
  {
      val now = Calendar.getInstance().getTime()
      val format = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
      val time = format.format(now)
      val dim = datas.take(1)(0).toArray.length
      val datasWithIndex = datas.zipWithIndex.map(t => (t._2, t._1))

      val path: String = outputDir + "/EXP-" + time + "/"
      s"mkdir -p ${path}".!
    
      val mapMin = Array.fill[Byte](dim)(0).mkString(",")
      var header = "# mapDim=2 mapSize={"+ nbRowSOM +"," + nbColSOM + "}"
      header += " pointDim=" + dim + " pointRealDim=" + dim + " mapMin={" + mapMin + "}"
    
      val prototypes = model.prototypes.map(d => (d.id, d._point)).sortBy(_._1).map(_._2)
      println("Write Prototypes...")
      val protosString = prototypes.map(d => d.toArray.mkString(",")).mkString("\n")

      // Utiliser fileWriter
      saveStr(path, header + "\n" + protosString, "maps")

      val sumAffectedDatas = datas.map(d => (model.findClosestPrototype(d).id, 1)).reduceByKey{ case (sum1, sum2) => sum1 + sum2 }.collectAsMap 
    
      // fill in all the prototypes that have 0 observations
      val card = (0 to prototypes.length - 1).map( d =>
      {
        if (sumAffectedDatas.contains(d))
        {
          sumAffectedDatas(d) + ""
        }
        else
        {
          "0"
        }
      })
    
      println("Write Cardinalities...")
      var cardHeader = "# mapDim=2 mapSize={"+ nbRowSOM +"," + nbColSOM + "}" 
      cardHeader +=  "pointDim=1 pointRealDim=0 mapMin={0} mapMax={0}"
      val cardStr = card.mkString("\n")
      saveStr(path, cardHeader + "\n" + cardStr, "cards")

      val affHeader = "# mapDim=1 mapSize={" + datas.count() + "} pointDim=1 pointRealDim=0 mapMin={0} mapMax={0}"
      val aff = datasWithIndex.map(d => (d._1, model.findClosestPrototype(d._2).id + "")).sortByKey().values.collect.mkString("\n")

      println("Write Affiliate...")
      saveStr(path, affHeader + "\n" + aff, "affs")    
      println("Write Maps...")

      val maps = prototypes.zip(card).map(d => d._1.toArray.mkString(",") + "," + d._2).mkString("\n")
      saveStr(path, maps, "mapscard")
      println("Write successfully...")
      time
  }
}

