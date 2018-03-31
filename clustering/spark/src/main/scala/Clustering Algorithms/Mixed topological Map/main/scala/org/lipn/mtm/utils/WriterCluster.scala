package clustering4ever.spark.clustering.mtm.utils

import org.apache.spark.mllib.linalg.DenseVector
import java.io._
import org.apache.spark.rdd.RDD
import clustering4ever.spark.clustering.mtm.global.AbstractModel


object WriterClusters {
  def js(data: RDD[NamedVector], model: AbstractModel, path: String) = {
    val writer = new PrintWriter(new File(path))

    val dataArray = data.collect
    var str = "var dataset = ["

    dataArray.foreach {d =>
      val closestNeuron = model.findClosestPrototype(d)
      if (d != dataArray.head) str += ','
      str += d.toJSON(closestNeuron.id)
    }

   
    str += "];"
    writer.write(str)

    writer.close()
  }
}
