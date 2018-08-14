package clustering4ever.spark.clustering.clusterwise

import _root_.scala.math.{pow, sqrt => ssqrt}
import _root_.scala.collection.mutable.ArrayBuffer
import _root_.scala.collection.mutable.HashMap
import _root_.scala.collection.immutable
import _root_.clustering4ever.util.SumArrays
import breeze.linalg._

trait CommonPLSTypes
{
	type IdWithX = ArrayBuffer[(Int, immutable.Seq[Double])]
	type Y = ArrayBuffer[immutable.Seq[Double]]
}

class PLS(
	dsXi: ArrayBuffer[(Int, immutable.Seq[Double])],
	dsY: ArrayBuffer[immutable.Seq[Double]],
	n: Int,
	h: Int,
	lw: Double,
	ktabXdudiY: (Int, Double, Double)
) extends AbstractRegression with CommonPLSTypes
{
	def reg() = 
	{
		val (idxDsX, dsX) = dsXi.unzip
		//val dsX = dsXi.map(_._2)
		//val idxDsX = dsXi.map(_._1)
		val ncolX = dsX.head.size
		val nrowX = dsX.size
		val ncolY = dsY.head.size
		val lw = 1D / n
		val meanX = dsX.reduce(SumArrays.sumArraysNumerics[Double](_, _)).map(_ / n).toArray
		val meanY = dsY.reduce(SumArrays.sumArraysNumerics[Double](_, _)).map(_ / n).toArray


		val yK = dsY.map( _.zip(meanY).map{ case (y, meanY) => y - meanY } )
		val xK = dsX.map( _.zip(meanX).map{ case (x, meanX) => x - meanX } )

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


		var bxK = DenseMatrix(xK:_*)
		val bYk = DenseMatrix(yK:_*)

		for( i <- 0 until maxdim )
		{
		  val yKt = yK.transpose
		  val bXklw = bxK.map(_ * lw)
		  val bXkt = bxK.t
		  val bYkt = DenseMatrix(yKt:_*)
		  val bXklwT = bXklw.t
		  val crossprod1 = bYkt * bXklw
		  val crossprod2 = crossprod1 * crossprod1.t
		  val eigen = eigSym(crossprod2)
		  val eigenVectors = eigen.eigenvectors.map( - _)
		  val eigenValues = eigen.eigenvalues.toArray.zipWithIndex.sortBy(- _._1)
		  val rescov2in = ArrayBuffer.empty[Double]
		  var covutk = 0D
		  

		  for( k <- 0 to nblo )
		  {
		    reseig += eigenValues(0)._1
		    resYc1 += eigenVectors(::, eigenValues(0)._2)
		    reslY += bYk * resYc1(i)
		    resTc1 += bXklwT * reslY(i)
		    resTc1(i) =
		    {
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
		  val positive = rsIdx.filter( _._1 > maxtol ).map(_._2).toArray

		  val ginv = if( positive.size == rsIdx.size )
		  {
		    val rdScala = rd.toArray
		    val trutScala = rut.toArray.grouped(rut.rows).toArray.transpose
		    val rowWiseProduct = rdScala.zip(trutScala).map{ case(multiplier, row) => row.map(_ * multiplier) }
		    val breezeRowWiseProduct = DenseMatrix(rowWiseProduct:_*)
		    v.t * breezeRowWiseProduct
		  }
		  else if( positive.size == 0 )
		  {
		  	DenseMatrix.zeros[Double](rsIdx.size, rsIdx.size)
		  }
		  else
		  {
		    val xsvdvKept = v.toArray.grouped(v.rows).toArray.map( row => for( keep <- positive ) yield (row(keep)) )
		    val breezeXsvdKeptRv = DenseMatrix(xsvdvKept:_*)

		    val rdScala = rd.toArray
		    val rdScalaKept = for( keep <- positive ) yield( rdScala(keep) )
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

		val resFax = ArrayBuffer(resW(0))
		val identityM = DenseMatrix.eye[Double](ncolX)
		var aA = identityM

		if( maxdim >= 2 )
		{
		  for( i <- 2 to maxdim )
		  {
		    val a0 = resl1(i - 2).t * bxK
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

		val resYcoli = scalaResYco.map(_.zipWithIndex.map{ case(v,idx) => v / normli(idx) })


		val rr = for( i <- 0 until ncolY ) yield for(j<- 0 until resFax.size) yield resFax(j).map(_*resYcoli(i)(j))

		val resXYcoef = for( i <- 0 until ncolY ) yield(
		{
	  		var cum = DenseVector.zeros[Double](rr(0)(0).size)
	  		for(j<- 0 until resFax.size) yield(
	  		{

			    val res = rr(i)(j) + cum
			    cum = rr(i)(j)
			    res
	  		})
		})

		val dataXb = DenseMatrix(dsX:_*)
		val dataYb = DenseMatrix(dsY:_*)

		val arrayRange1 = (0 until ncolY).toVector

		val resXYcoefBreeze = for( i <- arrayRange1 ) yield new DenseMatrix(rows = ncolX, cols = maxdim, resXYcoef(i).toArray.flatMap(_.toArray))

		val resIntercept = for( i <- arrayRange1 ) yield (DenseVector(meanX).t * resXYcoefBreeze(i)).t.map(meanY(i) - _)

		val resFitted = for( i <- arrayRange1 ) yield DenseMatrix(Array.fill(nrowX)(resIntercept(i).toArray):_*) + ( dataXb * resXYcoefBreeze(i) )

		val residuals = for( i <- arrayRange1 ) yield DenseMatrix(Array.fill(maxdim)(dataYb(::,i).toArray):_*).t - resFitted(i)

		val sumResidualSq = for( i <- 0 until ncolY ) yield(
		{ 
		  val squared = residuals(i).map(pow(_, 2))
		  sum(squared(::, *))
		})

		val rescritregmat = DenseMatrix((for( j <- 0 until sumResidualSq.size ) yield sumResidualSq(j).t.toArray).toArray:_* )
		val resCritReg = sum( rescritregmat(::, *) )

		val resCritRegHopt = resCritReg.t.toVector.apply(h - 1)

		val arrayRange2 = (0 until ncolY).toArray

		val colss = for( i <- arrayRange2 ) yield resXYcoefBreeze(i)(::, h - 1)
		val resXYcoefF = new DenseMatrix(rows = resXYcoefBreeze(0).rows, cols = ncolY, colss.flatMap(_.toArray) )
		val resInterceptF = resIntercept.map(_.toArray.last)
		val colsss = for( i <- arrayRange2 ) yield resFitted(i)(::,h - 1)
		val resFittedF = new DenseMatrix(rows = resFitted(0).rows, cols = ncolY, colsss.flatMap(_.toArray) )
		val resFittedFscala = for( i <- (0 until resFittedF.rows).toVector) yield (idxDsX(i), resFittedF(i,::).t.toArray)

		(resCritRegHopt, resXYcoefF, resInterceptF, resFittedFscala)
	}
}


object PLS extends CommonPLSTypes
{
	def runPLS(dsX: immutable.Vector[IdWithX], dsY: immutable.Vector[Y], g: Int, h: Int) =
	{
		val n = dsX(g).size
		val ktabXdudiYval = ktabXdudiY(dsX(g), dsY(g), n)
		val lw = 1D / n
		val mbplsObj = new PLS(dsX(g), dsY(g), n, h, lw, ktabXdudiYval)
		mbplsObj.reg().asInstanceOf[(Double, breeze.linalg.DenseMatrix[Double], Array[Double], immutable.Vector[(Int, immutable.Vector[Double])])]
	}

	def runFinalPLS(dsX: IdWithX, dsY: Y, lw: Double, n: Int, h: Int, ktabXdudiY: (Int, Double, Double)) =
	{
		val mbplsObj = new PLS(dsX, dsY, n, h, lw, ktabXdudiY)
		mbplsObj.reg().asInstanceOf[(Double, breeze.linalg.DenseMatrix[Double], Array[Double], immutable.Vector[(Int, immutable.Vector[Double])])]
	}

	def ktabXdudiY(dsX: IdWithX, dsY: Y, n: Int): (Int, Double, Double) =
	{
		val lw = 1D / n
		val cw = dsX.head._2.size
		val colw = dsY.head.size
		val dsY0 = dsY.map(_.head)
		val roww = dsY0.map( x => 1D / n )
		val ds = dsY0.zip(roww).map( x => x._1 * ssqrt(x._2))
		val eigValue = ds.map(pow(_, 2)).sum
		(cw, lw, eigValue)
	}


}

