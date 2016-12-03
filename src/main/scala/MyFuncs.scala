import java.io.{BufferedReader, File, FileReader}

/**
  * Created by Zetrov on 2016-11-03.
  */
class MyFuncs {

  /**
    *
    * @param file get Matrix data from File
    * @return return an array contains 'Row' and 'Column'
    */
  def getRowAndColumn(file: String): Array[Int] = {
    val res = new Array[Int](2)
    res(0) = 0
    res(1) = 0
    val fInput = new File(file)
    val fileReader = new FileReader(fInput)
    val reader = new BufferedReader(fileReader)

    var line = reader.readLine()
    while (line != null) {
      val data = line.split("\t")
      require(3 == data.length)
      //get max Row and Column in the Matrix
      if (res(0) < data(0).toInt)
        res(0) = data(0).toInt
      if (res(1) < data(1).toInt)
        res(1) = data(1).toInt
      line = reader.readLine()
    }
    reader.close()
    res(0) += 1
    res(1) += 1
    res
  }

  /**
    * get this function from MatrixRank Java class in Java folder
    *
    * @param matrix input matrix
    * @param row row of input matrix
    * @param col column of input matrix
    * @param error ???
    * @return return rank of input matrix
    */
  def getRank(matrix: Matrix, row: Int, col: Int, error: Int): Int = {
    var n = matrix.getColNum()
    var m = matrix.getRowNum()
    var i = 0
    var j = 0
    var i1 = 0
    var j1 = 0
    var temp1 = 0.toDouble

    // if row number larger than column number, set i = 1
    if (m > n) {
      i = m
      m = n
      n = i
      i = 1
    }
    val tmpMatrix = new Matrix(row, col)

    // if row number is larger than column number
    if (i == 0) {
      for (i <- 0 until m) {
        for (j <- 0 until n) {
          tmpMatrix.set(i, j, matrix.get(i, j))
        }
      }
    } else {
      // if column number is larger than row number
      for (i <- 0 until m) {
        for (j <- 0 until n) {
          tmpMatrix.set(i, j, matrix.get(j, i))
        }
      }
    }

    var error0 = 0.toDouble
    if (error == -1) {
      error0 = math.pow(0.1, 10)
    } else {
      error0 = math.pow(0.1, error)
    }

    i = 0
    while (i < m) {
      j = 0
      while (j < n) {
        if (tmpMatrix.get(i, j) != 0) {
          error0 *= tmpMatrix.get(i, j)
          // break out i and j loops
          i = m
          j = n
        }
        j += 1
      }
      i += 1
    }

    i = 0
    var error1 = 0.toDouble
    while(i < m) {
      j = 0
      var tmp = 0
      while (tmp < n) {
        if (tmpMatrix.get(i, tmp) != 0) {
          tmp = n
        }
        j += 1
        tmp += 1
      }

      if(j < n){
        i1 = 0
        while(i1 < m) {
          if (tmpMatrix.get(i1, j) != 0 && i1 != i) {
            temp1 = tmpMatrix.get(i, j) / tmpMatrix.get(i1, j)
            error1 = math.abs(tmpMatrix.get(i, j) - tmpMatrix.get(i1, j) * temp1) * 100
            error1 += error0
            j1 = 0
            while(j1 < n) {
              tmpMatrix.set(i1, j1, tmpMatrix.get(i, j1) - tmpMatrix.get(i1, j1) * temp1)
              if (math.abs(tmpMatrix.get(i1, j1)) < error1)
                tmpMatrix.set(i1, j1, 0)
              j1 += 1
            }
          }
          i1 += 1
        }
      }
      i += 1
    }

    i1 = 0
    i = 0
    for(i <- 0 until m) {
      j = 0
      while (j < n) {
        if (tmpMatrix.get(i, j) != 0) {
          i1 += 1
          j = n
        }
        j += 1
      }
    }
    i1
  }
}
