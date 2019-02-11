package org.clustering4ever.utils
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, mutable}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
object SortGsCz extends Serializable {
	/**
	 *
	 */
	final def sortByID[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]) = {
        val sorted = data.seq.sortBy(_.id)
        val builder = data.genericBuilder[Cz[O, V]].asInstanceOf[mutable.Builder[Cz[O, V], GS[Cz[O, V]]]]
        builder.sizeHint(data.size)
        builder ++= sorted
        builder.result
    }

}