import java.io._
import java.util
import java.util.{Arrays, Date, Random}

object amplify {
  def main(args: Array[String]): Unit = {

    val numberOfRecordsWanted: Int = 400
    val numberOfVariables: Int = 10
    val matrix = Array.ofDim[Int](numberOfRecordsWanted, numberOfVariables)
    var x: Int = 0
    var y: Int = 0
    var numberofRecordsInintialDataset: Int = 0


    try {

      val bufferedSource = io.Source.fromFile("/Users/Omal/Desktop/context-based-data-amp/src/main/inputDataset/Workbook1.csv")("UTF-8")

      for (line <- bufferedSource.getLines) {
        numberofRecordsInintialDataset += 1
        val values = line.split(",").map(_.trim)

        for (str <- values) {
          var str_int: Int = 0

          try {
            str_int = str.toInt
          } catch {
            case nfe: NumberFormatException =>
              str_int = 0
              System.out.println("null cell detected")
          }
          matrix(x)(y) = str_int
          //println(matrix(x)(y) + " ");
          y = y + 1 //you have inserted a value to the former y, need to increment

        }
        x = x + 1 // finished the row, need to increment the row number
        y = 0

      }
      x = x + 1
      bufferedSource.close




      /*
      *  ***************** Calculations  *****************
      */

      val selectedColumn = new Array[Int](numberofRecordsInintialDataset)
      var w: Int = 0
      val rand: Random = new Random

      /* Note that we are considering column wise now (downwards)*/
      var v: Int = 0
      while ({v < numberOfVariables }) {
        w = 0
        while ({w < numberofRecordsInintialDataset }) {
          selectedColumn(w) = matrix(w)(v)
          //println("@@" + selectedColumn(w));

          {
            w += 1;
            w - 1
          }
        }
        util.Arrays.sort(selectedColumn)
        val rangeMaximum: Int = selectedColumn(selectedColumn.length - 1)
        val rangeMinimum: Int = selectedColumn(0)
        //println("Maximum : " + rangeMaximum );
        //println("Minimum : " + rangeMinimum );

        /* Assigning random Values to particular column, starting right after the last record of the initial dataset*/ var m: Int = numberofRecordsInintialDataset
        while ( {
          m < numberOfRecordsWanted
        }) {
          matrix(m)(v) = rand.nextInt((rangeMaximum - rangeMinimum) + 1) + rangeMinimum

          {
            m += 1;
            m - 1
          }
        }
        //break;

        {
          v += 1;
          v - 1
        }
      }


      /*
      * ***************** Writing to fle *****************
      */


      println("writing file Started")
      val date: Date = new Date
      val builder: StringBuilder = new StringBuilder
      var i: Int = 0
      while ({i < numberOfRecordsWanted}) {
        //for each row
        var j: Int = 0
        while ( {
          j < numberOfVariables
        }) { //for each column
          builder.append(matrix(i)(j) + "") //append to the output string
          //println(matrix[i][j]);

          if (j < matrix.length - 1) { //if this is not the last row element
            builder.append(",") //then add comma to seperate
          }

          {
            j += 1;
            j - 1
          }
        }
        builder.append("\n") //append new line at the end of the row


        {
          i += 1;
          i - 1
        }
      }
      val writer: BufferedWriter = new BufferedWriter(new FileWriter("/Users/Omal/Desktop/context-based-data-amp/src/main/outputDataset/dataset" + date + ".csv"))
      writer.write(builder.toString) //save the string representation of the board

      writer.close()
      System.out.println("writing file finished")

    } catch {
      case ioException: IOException =>

    }
    //println(numberofRecordsInintialDataset);

  }
}
