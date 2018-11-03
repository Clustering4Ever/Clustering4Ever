package clustering4ever.spark.clustering.clusterwise

import scala.language.higherKinds
import scala.math.{pow, sqrt => ssqrt}
import scala.collection.{immutable, mutable, GenSeq}
import clustering4ever.util.SumVectors
import breeze.linalg.{DenseVector, DenseMatrix, Transpose, eigSym, svd, sum, *, diag}
import clustering4ever.util.VectorsBasicOperationsImplicits._
/**
 *
 */
trait CommonPLSTypes {
	type IdWithX[V <: Seq[Double]] = mutable.ArrayBuffer[(Int, V)]
	type Y[V <: Seq[Double]] = mutable.ArrayBuffer[V]
}
/**
 *
 */
class PLS[V[Double] <: Seq[Double]](
	dsXi: GenSeq[(Int, V[Double])],
	dsY: GenSeq[V[Double]],
	n: Int,
	h: Int,
	lw: Double,
	ktabXdudiY: (Int, Double, Double)
) extends AbstractRegression with CommonPLSTypes {
	def regression: (Double, DenseMatrix[Double], Array[Double], immutable.IndexedSeq[(Int, Array[Double])]) = {
		val (idxDsX, dsX) = dsXi.unzip
		val ncolX = dsX.head.size
		val nrowX = dsX.size
		val ncolY = dsY.head.size
		val lw = 1D / n
		val meanX = dsX.reduce(SumVectors.sumVectors(_, _).asInstanceOf[V[Double]]).map(_ / n)
		val meanY = dsY.reduce(SumVectors.sumVectors(_, _).asInstanceOf[V[Double]]).map(_ / n)
		val meanYa = meanY.toArray


		val yK = dsY.map(_.zip(meanY).map{ case (y, meanY) => y - meanY })
		val xK = dsX.map(_.zip(meanX).map{ case (x, meanX) => x - meanX })

		val maxdim = h
		val nblo = 0
		val reseig = mutable.ArrayBuffer.empty[Double]
		val resYc1 = mutable.ArrayBuffer.empty[DenseVector[Double]]
		val reslY = mutable.ArrayBuffer.empty[DenseVector[Double]]
		val resTc1 = mutable.ArrayBuffer.empty[DenseVector[Double]]
		val resl1 = mutable.ArrayBuffer.empty[DenseVector[Double]]
		val resTlX = mutable.ArrayBuffer.empty[DenseVector[Double]]
		val resW = mutable.ArrayBuffer.empty[DenseVector[Double]]
		val reslX = mutable.ArrayBuffer(DenseVector.zeros[Double](n))
		val resCritComp = mutable.ArrayBuffer.empty[Double]
		val ak = mutable.ArrayBuffer.empty[Double]
		val rescov2 = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Double]]


		var bxK = DenseMatrix(xK.seq:_*)
		val bYk = DenseMatrix(yK.seq:_*)
		val yKt = yK.transpose.map(_.seq).seq
		val bYkt = DenseMatrix(yKt:_*)

		for( i <- 0 until maxdim ) {
			val bXklw = bxK.map(_ * lw)
			val bXkt = bxK.t
			val bXklwT = bXklw.t
			val crossprod1 = bYkt * bXklw
			val crossprod2 = crossprod1 * crossprod1.t

			val eigen = eigSym(crossprod2)
			val eigenVectors = eigen.eigenvectors.map( - _)
			val eigenValues = eigen.eigenvalues.toArray.zipWithIndex.sortBy(_._1)(Ordering[Double].reverse)
			val rescov2in = mutable.ArrayBuffer.empty[Double]
			var covutk = 0D
		  
			for( k <- 0 to nblo ) {
				reseig += eigenValues(0)._1
				resYc1 += eigenVectors(::, eigenValues(0)._2)
				reslY += bYk * resYc1(i)
				resTc1 += bXklwT * reslY(i)
				resTc1(i) = {
					val tmpA = resTc1(i).toArray
					val sqrtsum = ssqrt(tmpA.map( x => x * x ).sum )
					DenseVector(tmpA.map(_ / sqrtsum))
				}
				resTlX += bxK * resTc1(i)
				covutk = reslY(i).map(_ * lw).t * resTlX(i)
				rescov2in += covutk * covutk
			}
			
			rescov2 += rescov2in
			
			for( k <- 0 to nblo ) {
				ak += covutk / ssqrt(rescov2(i).sum)
				reslX += resTlX(i).map(_ * ak(i))
			}

			val resl1tmp = reslX(i + 1).t * reslX(i + 1)
			resl1 += reslX(i + 1).map(_ / ssqrt(resl1tmp))


			val crossprodBxK = bXkt * bxK
			val tol = 1.490116 * math.pow(10, -8)
			val svd.SVD(u, s, v)  = svd(crossprodBxK)

			val rut = u.t
			val rd = s.map(1D / _)
			val maxtol = math.max(tol * s(0), 0)
			val rsIdx = s.toArray.zipWithIndex
			val positive = rsIdx.collect{ case (x, y) if x > maxtol => y }.toArray

			val ginv = if( positive.size == rsIdx.size ) {
				val rdScala = rd.toArray
				val trutScala = rut.toArray.grouped(rut.rows).toArray.transpose
				val rowWiseProduct = rdScala.zip(trutScala).map{ case(multiplier, row) => row.map(_ * multiplier) }
				val breezeRowWiseProduct = DenseMatrix(rowWiseProduct:_*)
				v.t * breezeRowWiseProduct
			}
			else if( positive.size == 0 ) DenseMatrix.zeros[Double](rsIdx.size, rsIdx.size)
			else {
				val xsvdvKept = v.toArray.grouped(v.rows).toArray.map( row => for( keep <- positive ) yield (row(keep)) )
				val breezeXsvdKeptRv = DenseMatrix(xsvdvKept:_*)

				val rdScala = rd.toArray
				val rdScalaKept = positive.map( keep => rdScala(keep) )
				val rutScala = rut.toArray.grouped(rut.rows).toArray.map( row => for( keep <- positive ) yield row(keep) )
				val trutScalaKept = rutScala.map( row => for( keep <- positive ) yield row(keep) ).transpose
				val rowWiseProduct = rdScalaKept.zip(trutScalaKept).map{ case (multiplier, row) => row.map(_ * multiplier) }
				val breezeRowWiseProduct = DenseMatrix(rowWiseProduct:_*)
				breezeXsvdKeptRv * breezeRowWiseProduct
			}  

			val tCrossProdW = ginv * bXkt
			resW += tCrossProdW * reslX(i + 1)

			val crossProdXY = bXklwT * bYk
			val crossProdW = resW(i) * resW(i).t
			val pred_crit_comp = crossProdXY - crossProdW * crossProdXY
			val pred_crit_comp_squared = pred_crit_comp.map( x => x * x )
			val colSum = sum(pred_crit_comp_squared(::, *))
			resCritComp += ssqrt(colSum.t.toArray.sum)

			val wts = ssqrt(lw)
			val lX01 = reslX(i + 1)
			val lX01t = lX01.t
			val lX02 = lX01 * lX01t
			val lX03 = lX01t * lX01
			bxK = bxK - lX02.map(_ / lX03) * bxK
		}

		val resFax = mutable.ArrayBuffer(resW.head)

		if( maxdim >= 2 ) {
			val identityM = DenseMatrix.eye[Double](ncolX)

			def addToBuff(i: Int, m: DenseMatrix[Double]) = {
				val a0 = resl1(i - 2).t * bxK
				val a1 = ssqrt(reslX(i - 1).t * reslX(i - 1))
				val a = a0.t.map(_ / a1).t
				val res = m * ( identityM - (resW(i - 2) * a) )
				resFax += res * resW(i - 1)
				res
			}

			@annotation.tailrec
			def go(i: Int, m: DenseMatrix[Double]): DenseMatrix[Double] = {
				if( i < maxdim ) go(i + 1, addToBuff(i, m))
				else addToBuff(i, m)
			}

			go(2, identityM)
		}

		val reslXmat = new DenseMatrix(rows = nrowX, cols = reslX.size - 1, reslX.tail.toArray.flatMap(_.toArray))
		val reslXmatsqrtlw = reslXmat.map(_ * ssqrt(lw))
		val normli = diag( reslXmatsqrtlw.t * reslXmatsqrtlw ).toArray

		val resYco = bYk.t * diag(DenseVector(Array.fill(nrowX)(lw))) * reslXmat
		val scalaResYco = resYco.toArray.grouped(resYco.rows).toArray.transpose

		val resYcoli = scalaResYco.map(_.zipWithIndex.map{ case (v, idx) => v / normli(idx) })


		val faxRange = (0 until resFax.size)
		val rr = (0 until ncolY).map( i => faxRange.map( j => resFax(j).map(_ * resYcoli(i)(j)) ) )

		val resXYcoef = for( i <- 0 until ncolY ) yield {
	  		var cum = DenseVector.zeros[Double](rr.head.head.size)
	  		faxRange.map{ j =>
			    val res = rr(i)(j) + cum
			    cum = rr(i)(j)
			    res
	  		}
		}

		val dataXb = DenseMatrix(dsX.seq:_*)
		val dataYb = DenseMatrix(dsY.seq:_*)

		val arrayRange1 = (0 until ncolY).toVector

		val resXYcoefBreeze = arrayRange1.map( i => new DenseMatrix(rows = ncolX, cols = maxdim, resXYcoef(i).toArray.flatMap(_.toArray)) )

		val resIntercept = arrayRange1.map( i => (DenseVector(meanX.toArray).t * resXYcoefBreeze(i)).t.map(meanYa(i) - _) )

		val resFitted = arrayRange1.map( i => DenseMatrix(Array.fill(nrowX)(resIntercept(i).toArray):_*) + ( dataXb * resXYcoefBreeze(i) ) )

		val residuals = arrayRange1.map( i => DenseMatrix(Array.fill(maxdim)(dataYb(::,i).toArray):_*).t - resFitted(i) )

		val sumResidualSq = for( i <- 0 until ncolY ) yield { 
		  val squared = residuals(i).map( x => x * x )
		  sum(squared(::, *))
		}

		val regressionCriteriaMatrix = DenseMatrix((0 until sumResidualSq.size).map( j => sumResidualSq(j).t.toArray ).toArray:_* )
		val regressionCriteria: Transpose[DenseVector[Double]] = sum(regressionCriteriaMatrix(::, *))
		val regressionCriteriaHopt = regressionCriteria.apply(h - 1)

		val arrayRange2 = (0 until ncolY)
		val xyCoef = new DenseMatrix(rows = resXYcoefBreeze(0).rows, cols = ncolY, arrayRange2.flatMap( i => resXYcoefBreeze(i)(::, h - 1).toArray ).toArray)
		val interceptF = resIntercept.map(_.toArray.last).toArray
		val fittedF = new DenseMatrix(rows = resFitted(0).rows, cols = ncolY, arrayRange2.flatMap( i => resFitted(i)(::,h - 1).toArray ).toArray)
		val fittedFscala = (0 until fittedF.rows).map( i => (idxDsX(i), fittedF(i,::).t.toArray) )

		(regressionCriteriaHopt, xyCoef, interceptF, fittedFscala)
	}
}

object PLS extends CommonPLSTypes {

	def runClusterwisePLS[V[Double] <: Seq[Double]](dsX: Array[IdWithX[V[Double]]], dsY: Array[Y[V[Double]]], g: Int, h: Int): (Double, DenseMatrix[Double], Array[Double], immutable.IndexedSeq[(Int, Array[Double])]) = {
		val n = dsX(g).size
		val ktabXdudiYval = ktabXdudiY(dsX(g), dsY(g), n)
		val lw = 1D / n
		val mbplsObj = new PLS(dsX(g), dsY(g), n, h, lw, ktabXdudiYval)
		mbplsObj.regression
	}

	def runPLS[V[Double] <: Seq[Double]](dsX: IdWithX[V[Double]], dsY: Y[V[Double]], h: Int): (Double, DenseMatrix[Double], Array[Double], immutable.IndexedSeq[(Int, Array[Double])]) = {
		val n = dsX.size
		val lw = 1D / n
		val ktabXdudiYvalues = ktabXdudiY(dsX, dsY, n)
		val mbplsObj = new PLS(dsX, dsY, n, h, lw, ktabXdudiYvalues)
		mbplsObj.regression
	}

	def ktabXdudiY[V[Double] <: Seq[Double]](dsX: IdWithX[V[Double]], dsY: Y[V[Double]], n: Int): (Int, Double, Double) = {
		val lw = 1D / n
		val cw = dsX.head._2.size
		val colw = dsY.head.size
		val dsY0 = dsY.map(_.head)
		val roww = dsY0.map( x => 1D / n )
		val ds = dsY0.zip(roww).map{ case (x, y) => x * ssqrt(y) }
		val eigValue = ds.map( x => x * x ).sum
		(cw, lw, eigValue)
	}
}

