import java.io.{BufferedReader, File, FileReader, FileWriter}


/**
  * Created by Zetrov on 16/09/2016.
  */
class Matrix(private val row: Int, private val column: Int) {
  protected val pdValue = new Array[Array[Double]](row)
  for (i <- 0 until row)
    pdValue(i) = new Array[Double](column)

  def set(i: Int, j: Int, value: Double) { pdValue(i)(j) = value }
  def get(i: Int, j: Int): Double = pdValue(i)(j)
  def getRow(rowIdx: Int): Array[Double] = pdValue(rowIdx)
  def setRow(rowIdx: Int, value: Array[Double]): Unit = {
    require(column == value.length)
    for(colIdx <- value.indices)
      set(rowIdx, colIdx, value(colIdx))
  }
  def getRowNum() : Int = row
  def getColNum(): Int = column

  def createMatrixFromFile(sWorkingFolder: String, sInFileName: String): Unit = {
    val finput = new File(sWorkingFolder, sInFileName)
    val fileReader = new FileReader(finput)
    val reader = new BufferedReader(fileReader)

    var line = reader.readLine()
    while (line != null) {
      var data = line.split("\t")
      require(data.length == 3)
      set(data(0).toInt, data(1).toInt, data(2).toDouble)
      line = reader.readLine()
    }
    reader.close()
  }

  def createMatrixFromFile(sInFileName: String): Unit = {
    val fInput = new File(sInFileName)
    val fileReader = new FileReader(fInput)
    val reader = new BufferedReader(fileReader)

    var line = reader.readLine()
    while(line != null){
      val data = line.split("\t")
      require(3 == data.length)
      set(data(0).toInt, data(1).toInt, data(2).toDouble)
      line = reader.readLine()
    }
    reader.close()
  }

  def display(row: Int, column: Int): Unit = {
    for (i <- 0 until row) {
      for (j <- 0 until column)
        print("\t" + pdValue(i)(j))
      println()
    }
    println()
  }

  def display(): Unit = {
    for (i <- 0 until row) {
      for (j <- 0 until column)
        print("\t" + pdValue(i)(j))
      println()
    }
    println()
  }

  def add(X: Matrix): Unit = {
    for (rowIndex <- 0 until row)
      for (columnIndex <- 0 until column) {
        var item = X.get(rowIndex, columnIndex)
        set(rowIndex, columnIndex, this.get(rowIndex, columnIndex) + item)
      }
  }


  def outProducter(X: Matrix): Matrix = {
    val matrix1: Array[Array[Double]] = pdValue
    val matrix2: Array[Array[Double]] = X.pdValue
    val rows = matrix1.length
    val rows2 = matrix2.length
    val matrix = new Matrix(rows, rows2)

    val columns = matrix1(0).length
    var y = 0
    for (row <- 0 until rows) {
      for (row2 <- 0 until rows2) {
        var item = 0.0
        for (column <- 0 until columns){
          val sum = matrix1(row)(column) * matrix2(row2)(column)
          item = item + sum
        }
        matrix.set(row, y, item)
        y = y + 1
      }
      y = 0
    }
    matrix
  }

  def save(sWorkingFolder: String, sOutFileName: String): Unit = {
    val fOutput = new File(sWorkingFolder, sOutFileName)
    val fileWriterOutput = new FileWriter(fOutput)
    var strToWrite = ""

    for (rowIndex <- 0 until row) {
      for (columnIndex <- 0 until column) {
        strToWrite = rowIndex + "\t" + columnIndex + "\t" + pdValue(rowIndex)(columnIndex) + "\n"
        fileWriterOutput.append(strToWrite)
        fileWriterOutput.flush()
      }
    }
    fileWriterOutput.flush()
    fileWriterOutput.close()
  }
}


