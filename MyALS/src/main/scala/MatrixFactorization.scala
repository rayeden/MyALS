import scala.util.Random

/**
  * Created by Zetrov on 2016-11-01.
  */

/**
  * @param nRow      matrix row number
  * @param nColumn   matrix column number
  * @param nRank     rank of Matrix
  * @param sFilename file path and file name
  * @param dStopping stop iterating when the result is less than the dStopping (Converged)
  */
class MatrixFactorization(val nRow: Int,
                          val nColumn: Int,
                          val nRank: Int,
                          val sFilename: String,
                          //val X: Matrix,
                          val dStopping: Double) {
  var gdAlpha: Double = 0.001
  var dPreviousLoss: Double = 10E8

  val X = new Matrix(nRow, nColumn)
  X.createMatrixFromFile(sFilename)
  //init U and V randomly
  val U = initFactor(nRow, nRank)
  val V = initFactor(nColumn, nRank)

  /**
    * Function: factorize the Matrix
    */
  def factorize(): Unit = {
    var isConverged = false
    var currentLoss = 0.toDouble
    var iter = 0

    while (!isConverged) {

      println("iterate time: " + iter)
      iter += 1

//      Update U while fixing V
//      println("1. Update U")
      updateU()

//      Update V while fixing U
//      println("2. Update V")
      updateV()

//      Compute Loss
      currentLoss = computeLoss()
      println("Loss: \t" + currentLoss)

      val dLossDiff = dPreviousLoss - currentLoss
      println("dLossDiff: \t" + dLossDiff)

      if (dLossDiff < dStopping)
        isConverged = true

      dPreviousLoss = currentLoss
    }

    var result = new Matrix(nRow, nColumn)
    result = U.outProducter(V)
    result.display()

//    U.save("A:\\Quan\\BigData course", "U.txt")
//    V.save("A:\\Quan\\BigData course", "V.txt")
//    result.save("A:\\Quan\\BigData course", "result.txt")
  }

  def updateU(): Unit = {
    //Update rows in U
    for (rowIdx <- 0 until nRow) {
      updateU(rowIdx)
    }
  }

  def updateU(rowIdx: Int): Unit = {
    //Update every element in Urow
    for (r <- 0 until nRank) {
      val gradient = computeUGradient(rowIdx, r)
      val result = U.get(rowIdx, r) - gdAlpha * gradient
      U.set(rowIdx, r, result)
      //U.display()
    }
  }

  /**
    * compute the gradient of U matrix
    * take V values as constant
    *
    * @param rowIdx
    * @return return the derivation
    */
  def computeUGradient(rowIdx: Int, rank: Int): Double = {
    var gradient = 0.toDouble
    // get row from original Matrix X
    val x_row = X.getRow(rowIdx)
    for (j <- x_row.indices) {
      if (x_row(j) != 0.0) {
        var uv = 0.toDouble
        // x_ij - (ui1*vj1 + ui2*vj2 + .. + uiR*vjR)
        for (r <- 0 until nRank) {
          val u_i = U.get(rowIdx, r)
          val v_j = V.get(j, r)
          uv += u_i * v_j
        }
        gradient += (x_row(j) - uv) * V.get(j, rank)
      }
    }
    gradient *= -2
    gradient
  }

  def updateV(): Unit = {
    for (colIdx <- 0 until nColumn) {
      updateV(colIdx)
    }
  }

  def updateV(colIdx: Int): Unit = {
    for (r <- 0 until nRank) {
      val gradient = computeVGradient(colIdx, r)
      val result = V.get(colIdx, r) - gdAlpha * gradient
      V.set(colIdx, r, result)
      //V.display()
    }
  }

  def computeVGradient(colIdx: Int, rank: Int): Double = {
    var gradient = 0.toDouble
    for (i <- 0 until nRow) {
      val x_ij = X.get(i, colIdx)
      if(x_ij != 0.0) {
        var uv = 0.toDouble
        for (r <- 0 until nRank) {
          val u_i = U.get(i, r)
          val v_j = V.get(colIdx, r)
          uv += u_i * v_j
        }
        gradient += (x_ij - uv) * U.get(i, rank)
      }
    }
    gradient *= -2
    gradient
  }

  def computeLoss(): Double = {
    //var loss = new Matrix(nRow, nColumn)
    //loss = U.outProducter(V)
    var dLoss = 0.toDouble
    for (i <- 0 until nRow) {
      for (j <- 0 until nColumn) {
        if(X.get(i, j) != 0.0) {
          val u = U.getRow(i)
          val v = V.getRow(j)
          var uv_loss = 0.toDouble
          for(r <- 0 until nRank)
            uv_loss += u(r) * v(r)
//          val diff = X.get(i, j) - loss.get(i, j)
          val diff = X.get(i, j) - uv_loss
          dLoss += diff * diff
        }
      }
    }
    dLoss
  }

  /**
    * function: init a Matrix randomly
    *
    * @param row
    * @param column
    * @return
    */
  def initFactor(row: Int, column: Int): Matrix = {
    val factor = new Matrix(row, column)

    for (rowIdx <- 0 until row) {
      for (colIdx <- 0 until column) {
        val value = Random.nextDouble()
        factor.set(rowIdx, colIdx, value)
      }
    }
    factor
  }
}
