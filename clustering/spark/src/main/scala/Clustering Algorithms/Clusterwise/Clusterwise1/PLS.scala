package clustering4ever.spark.clustering.clusterwise

import scala.math.{pow, sqrt => ssqrt}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import breeze.linalg._

object Pls extends Serializable
{
	type DsWithIdAndX = ArrayBuffer[(Int, Array[Double])]
	type DsWithY = ArrayBuffer[Array[Double]]

	def ktabXdudiY(dsX: DsWithIdAndX, dsY: DsWithY, n0: Int): (Int,Double,Double) =
	{
		val lw = 1D / n0
		val cw = dsX.head._2.size
		val colw = dsY.head.size
		val dsY0 = dsY.map(_(0))
		val roww = dsY0.map( x => 1D / n0 )
		val ds = dsY0.zip(roww).map(x => x._1 * ssqrt(x._2))
		val eigValue = ds.map( pow(_, 2) ).reduce(_ + _)
		(cw, lw, eigValue)
	}


	def prepareAndLaunchPls(dsX: Array[DsWithIdAndX], dsY: Array[DsWithY], g: Int, h: Int) = 
	{
		val n = dsX(g).size
		val ktabXdudiYVal = ktabXdudiY(dsX(g), dsY(g), n)
		pls(dsX(g), dsY(g), n, h, ktabXdudiYVal)
	}


	def pls(dsXi: DsWithIdAndX, dsY: DsWithY, n: Int, h: Int, ktabXdudiY0: (Int, Double, Double)) = 
	{
		val dsX = dsXi.map(_._2)
		val idxDsX = dsXi.map(_._1)
		val ncolX = dsX.head.size
		val nrowX = dsX.size
		val ncolY = dsY.head.size
		val lw = 1D / n
		val meanX = dsX.reduce( _.zip(_).map( x=> x._1 + x._2) ).map(_ / n)
		val meanY = dsY.reduce( _.zip(_).map( x=> x._1 + x._2) ).map(_ / n)

		val yK = dsY.map( _.zip(meanY).map( x => (x._1 - x._2) ) )
		val xK = dsX.map( _.zip(meanX).map( x => (x._1 - x._2) ) )

		val maxdim = h
		val nblo = 0
		val reseig = ArrayBuffer.empty[Double]
		val resYc1 = ArrayBuffer.empty[DenseVector[Double]]
		val reslY = ArrayBuffer.empty[DenseVector[Double]]
		val resTc1 = ArrayBuffer.empty[DenseVector[Double]]
		val resl1 = ArrayBuffer.empty[DenseVector[Double]]
		val resTlX = ArrayBuffer.empty[DenseVector[Double]]
		val resW = ArrayBuffer.empty[DenseVector[Double]]
		val reslX = ArrayBuffer(DenseVector.zeros[Double](n))
		val resCritComp = ArrayBuffer.empty[Double]
		val ak = ArrayBuffer.empty[Double]
		val rescov2 = ArrayBuffer.empty[ArrayBuffer[Double]]

		val bxK0 = DenseMatrix(xK:_*)
		var bxK = bxK0
		val bYk = DenseMatrix(yK:_*)

		for( i <- 0 until maxdim )
		{
		  val yKt = yK.transpose
		  val bXklw = bxK.map(_*lw)
		  val bYkt = DenseMatrix(yKt:_*)
		  val crossprod1 = bYkt * bXklw
		  val crossprod2 = crossprod1 * crossprod1.t
		  val eigen = eigSym(crossprod2)
		  val eigenVectors = eigen.eigenvectors.map( - _)
		  val eigenValues = eigen.eigenvalues.toArray.zipWithIndex.sortBy(_._1).reverse
		  val rescov2in = ArrayBuffer.empty[Double]
		  var covutk = 0D
		  
		  for( k <- 0 to nblo )
		  {
		    reseig += eigenValues.head._1
		    resYc1 += eigenVectors(::, eigenValues.head._2)
		    reslY += bYk * resYc1(i)
		    resTc1 += bXklw.t * reslY(i)
		    resTc1(i) = {
		      val sqrtsum = ssqrt(resTc1(i).toArray.map( x => scala.math.pow(x, 2)).sum )
		      DenseVector(resTc1(i).toArray.map(_ / sqrtsum))
		    }
		    resTlX += bxK * resTc1(i)
		    covutk = reslY(i).map(_ * lw).t * resTlX(i)
		    rescov2in += scala.math.pow(covutk, 2)
		  }
		  rescov2 += rescov2in
		  for( k <- 0 to nblo )
		  {
		    ak += covutk / ssqrt(rescov2(i).reduce(_+_))
		    reslX += resTlX(i).map(_ * ak(i))
		  }

		  val resl1tmp = reslX(i + 1).t * reslX(i + 1)
		  resl1 += reslX(i + 1).map(_ / ssqrt(resl1tmp))
		  
		  val crossprodBxK = bxK.t * bxK

		  val tol = 1.490116 * math.pow(10, -8)
		  val ginvCrossProdBxK = svd(crossprodBxK)

		  val rut = ginvCrossProdBxK.U.t
		  val rd = ginvCrossProdBxK.S.map(1D / _)
		  val rs = ginvCrossProdBxK.S
		  val rv = ginvCrossProdBxK.Vt
		  val maxtol = math.max(tol * rs(0), 0)
		  val rsIdx = rs.toArray.zipWithIndex
		  val positive = rsIdx.filter( _._1 > maxtol ).map(_._2).toArray

		  val ginv = if( positive.size == rsIdx.size )
		  {
		    val rdScala = rd.toArray
		    val trutScala = rut.toArray.grouped(rut.rows).toArray.transpose
		    val rowWiseProduct = rdScala.zip(trutScala).map{ case(multiplier, row) => row.map(_*multiplier) }
		    val breezeRowWiseProduct = DenseMatrix(rowWiseProduct:_*)
		    rv.t * breezeRowWiseProduct
		  }
		  else if( positive.size == 0 ) DenseMatrix.zeros[Double](rsIdx.size, rsIdx.size)
		  else
		  {
		    val xsvdvKept = rv.toArray.grouped(rv.rows).toArray.map( row => for( keep <- positive ) yield (row(keep)) )
		    val breezeXsvdKeptRv = DenseMatrix(xsvdvKept:_*)

		    val rdScala = rd.toArray
		    val rdScalaKept = for( keep <- positive ) yield (rdScala(keep))
		    val rutScala = rut.toArray.grouped(rut.rows).toArray.map( row => for( keep <- positive ) yield(row(keep)) )
		    val trutScalaKept = rutScala.map( row => for( keep <- positive ) yield (row(keep)) ).transpose
		    val rowWiseProduct = rdScalaKept.zip(trutScalaKept).map{ case (multiplier, row) => row.map(_ * multiplier) }
		    val breezeRowWiseProduct = DenseMatrix(rowWiseProduct:_*)

		    breezeXsvdKeptRv * breezeRowWiseProduct
		  }  
		  
		  val tCrossProdW = ginv * bxK.t
		  resW += tCrossProdW * reslX(i + 1)

		  val crossProdXY = bxK.map(_ * lw).t * bYk
		  val crossProdW = resW(i) * resW(i).t
		  val predCritComp = crossProdXY - crossProdW * crossProdXY
		  val predCritComp_squared = predCritComp.map( x => x * x )
		  val colSum = sum(predCritComp_squared(::, *))
		  resCritComp += ssqrt(colSum.t.sum)
		  
		  val wts = ssqrt(lw)
		  val lX01 = reslX(i + 1)
		  val lX01t = lX01.t
		  val lX02 = lX01 * lX01t
		  val lX03 = lX01t * lX01
		  bxK = bxK - lX02.map( _ / lX03 ) * bxK
		}

		val resFax = ArrayBuffer(resW(0))
		val identityM = DenseMatrix.eye[Double](ncolX)
		var aA = identityM

		if( maxdim >= 2 )
		{
		  for( i <- 2 to maxdim )
		  {
		    val a0 = resl1(i - 2).t * bxK0
		    val a1 = ssqrt(reslX(i - 1).t * reslX(i - 1))
		    val a = a0.t.map(_ / a1).t
		    aA = aA * ( identityM - (resW(i - 2) * a) )
		    resFax += aA * resW(i - 1)
		  }
		}

		val reslXmat = new DenseMatrix(rows = nrowX, cols = reslX.size - 1, reslX.tail.toArray.flatMap(_.toArray))
		val reslXmatsqrtlw = reslXmat.map(_ * ssqrt(lw))
		val normli = diag( reslXmatsqrtlw.t * reslXmatsqrtlw ).toArray

		val resYco = bYk.t * diag(DenseVector(Array.fill(nrowX)(lw))) * reslXmat
		val scalaResYco = resYco.toArray.grouped(resYco.rows).toArray.transpose

		val resYcoli = scalaResYco.map(_.zipWithIndex.map{ case(v, idx) => v / normli(idx) })


		val rr = for( i <- 0 until ncolY ) yield (for(j<- 0 until resFax.size) yield (resFax(j).map(_ * resYcoli(i)(j))))

		val resXYcoef = for( i <- 0 until ncolY ) yield (
		{
	  		var cum = DenseVector.zeros[Double](rr(0)(0).size)
	  		for( j <- 0 until resFax.size ) yield (
	  		{
			    val res = rr(i)(j) + cum
			    cum = rr(i)(j)
			    res
	  		})
		})

		val dataXb = DenseMatrix(dsX:_*)
		val dataYb = DenseMatrix(dsY:_*)

		val arrayRange1 = (0 until ncolY).toArray

		val resXYcoefBreeze = for( i <- arrayRange1 ) yield (new DenseMatrix(rows=ncolX, cols = maxdim, resXYcoef(i).toArray.flatMap(_.toArray)))

		val resIntercept = for( i <- arrayRange1 ) yield ((DenseVector(meanX).t * resXYcoefBreeze(i)).t.map(meanY(i)-_))

		val resFitted = for( i <- arrayRange1 ) yield (DenseMatrix(Array.fill(nrowX)(resIntercept(i).toArray):_*) + ( dataXb * resXYcoefBreeze(i) ))

		val residuals = for( i <- arrayRange1 ) yield (DenseMatrix(Array.fill(maxdim)(dataYb(::,i).toArray):_*).t - resFitted(i))

		val sumResidualSq = for( i <- 0 until ncolY ) yield(
		{ 
		  val squared = residuals(i).map( x => x * x )
		  sum(squared(::,*))
		})

		val rescritregmat = DenseMatrix((for( j <- (0 until sumResidualSq.size).toArray ) yield (sumResidualSq(j).t.toArray)):_* )
		val resCritReg = sum( rescritregmat(::, *) )

		val resCritRegHopt = resCritReg.t.toArray.apply(h-1)
		val arrayRange2 = (0 until ncolY).toArray

		val colss = for( i <- arrayRange2 ) yield (resXYcoefBreeze(i)(::, h - 1))
		val resXYcoefF = new DenseMatrix(rows = resXYcoefBreeze.head.rows, cols = ncolY, colss.flatMap(_.toArray))
		val resInterceptF = resIntercept.map(_.toArray.last)
		val colsss = for( i <- arrayRange2 ) yield( resFitted(i)(::,h-1) )
		val resFittedF = new DenseMatrix(rows = resFitted.head.rows, cols = ncolY, colsss.flatMap(_.toArray))
		val resFittedFscala = for( i <- (0 until resFittedF.rows).toArray) yield ((idxDsX(i), resFittedF(i, ::).t.toArray))

		(resCritRegHopt, resXYcoefF, resInterceptF, resFittedFscala) 

	}
}