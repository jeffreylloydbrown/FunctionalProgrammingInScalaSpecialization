package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{
  DoubleType,
  StringType,
  StructField,
  StructType
}
import org.apache.spark.sql.functions._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import TimeUsage._

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("dfSchema conversion") {
    val schema = TimeUsage.dfSchema(List("A", "B", "C"))
    assert(schema(0).name === "A")
    assert(schema(0).dataType === StringType)
    assert(schema(0).nullable === false)
    assert(schema(1).name === "B")
    assert(schema(1).dataType === DoubleType)
    assert(schema(1).nullable === false)
    assert(schema(2).name === "C")
    assert(schema(2).dataType === DoubleType)
    assert(schema(2).nullable === false)
    assert(schema.length === 3)

    // A single column works, only has the StringType column.
    val single = dfSchema(List("D"))
    assert(single(0).name === "D")
    assert(single(0).dataType === StringType)
    assert(single(0).nullable === false)
    assert(single.length === 1)
  }

  test("dfSchema throws exception if given no columns list") {
    intercept[java.lang.IllegalArgumentException] {
      dfSchema(List())
    }
  }

  test("row conversion") {
    val row = TimeUsage.row(List("A", "12", "98.6", "-14.2", "0"))
    assert(row(0) === "A")
    assert(row(1) === 12D)
    assert(row(2) === 98.6)
    assert(row(3) === -14.2)
    assert(row(4) === 0D)
    assert(row.length === 5)

    // Confirm single column works, no exception.
    val single = TimeUsage.row(List("E"))
    assert(single(0) === "E")
    assert(single.length === 1)
  }

  test("row() throws exception if given no columns list") {
    intercept[java.lang.IllegalArgumentException] {
      row(List())
    }
  }

  test("read() can build the dataframe from the time usage data") {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    assert(columns.length > 0)
    assert(columns(6) === "teage")
    assert(columns(3) === "peeduca")
    //initDf.show()                 // uncomment to see first rows of CSV
    val rows: Array[Row] = initDf.take(2)  // does not contain column names
    assert(rows(0)(6) === 60.0)     // load CSV in Excel, cell G2 is 60.0
    assert(rows(1)(3) === 40.0)     // cell D3 is 40.0
  }

  test("classifiedColumns functionality") {
    val columnNames = List("skip", "t01", "t030", "t1801", "t1802", "skip", "t1803", "t18", "t1804",
                           "t051", "t1805")
    val (primary, working, other) = classifiedColumns(columnNames)

    assert((primary ++ working ++ other).contains(col("skip")) === false)
    assert(primary.contains(col("t01")) && !working.contains(col("t01")) && !other.contains(col("t01")))
    assert(primary.contains(col("t030")) && !working.contains(col("t030")) && !other.contains(col("t030")))
    assert(primary.contains(col("t1801")) && !working.contains(col("t1801")) && !other.contains(col("t1801")))
    assert(!primary.contains(col("t1802")) && !working.contains(col("t1802")) && other.contains(col("t1802")))
    assert(primary.contains(col("t1803")) && !working.contains(col("t1803")) && !other.contains(col("t1803")))
    assert(!primary.contains(col("t18")) && !working.contains(col("t18")) && other.contains(col("t18")))
    assert(!primary.contains(col("t1804")) && !working.contains(col("t1804")) && other.contains(col("t1804")))
    assert(!primary.contains(col("t051")) && working.contains(col("t051")) && !other.contains(col("t051")))
    assert(!primary.contains(col("t1805")) && working.contains(col("t1805")) && !other.contains(col("t1805")))
    assert(primary.length === 4)
    assert(working.length === 2)
    assert(other.length === 3)
  }

  val (columns, initDf) = read("/timeusage/simplified.csv")
  val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
  val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf).cache()

  test("timeUsageSummary functionality") {
    // Uncomment this to see the data for the asserts below.  There are 10 rows of data.
    // All the hour values are the same per column, and we are confirming the expected
    // results below.  If the contents of simplified.csv change, these tests have to
    // get updated.
    //summaryDf.show()

    val row_count = 10

    assert(summaryDf.count() === row_count)  // verifies we read the whole file
    // results should add to row_count
    assert(summaryDf.where("age == 'young'").count() === 1)
    assert(summaryDf.where("age == 'elder'").count() === 1)
    assert(summaryDf.where("age == 'active'").count() === 8)
    // results should add to row_count
    assert(summaryDf.where("working == 'working'").count() === 9)
    assert(summaryDf.where("working == 'not working'").count() === 1)
    // results should add to row_count
    assert(summaryDf.where("sex == 'male'").count() === 2)
    assert(summaryDf.where("sex == 'female'").count() === 8)
    // each row has the same value, so the count of them should be row_count
    assert(summaryDf.where("primaryNeeds == 1.5").count() === row_count)
    assert(summaryDf.where("work == 2.0").count() === row_count)
    assert(summaryDf.where("other == 7.0").count() === row_count)
  }

  val finalDf = timeUsageGrouped(summaryDf).cache()

  test("timeUsageGrouped functionality") {
    // Uncomment this to see the data for the asserts below.  There are 10 rows of data.
    // All the hour values are the same per column, and we are confirming the expected
    // results below.  If the contents of simplified.csv change, these tests have to
    // get updated.
    //finalDf.show()

    // Excel says
    // +-----------+------+------+------------+----+-----+
    // |    working|   sex|   age|primaryNeeds|work|other|
    // +-----------+------+------+------------+----+-----+
    // |not working|female|active|         1.5| 2.0|  7.0|
    // |    working|female|active|         1.5| 2.0|  7.0|
    // |    working|female| young|         1.5| 2.0|  7.0|
    // |    working|  male|active|         1.5| 2.0|  7.0|
    // |    working|  male| elder|         1.5| 2.0|  7.0|
    // +-----------+------+------+------------+----+-----+

    val row_count = 5

    assert(finalDf.count() === row_count)
    // results should add to row_count
    assert(finalDf.where("age == 'young'").count() === 1)
    assert(finalDf.where("age == 'elder'").count() === 1)
    assert(finalDf.where("age == 'active'").count() === 3)
    // results should add to row_count
    assert(finalDf.where("working == 'working'").count() === 4)
    assert(finalDf.where("working == 'not working'").count() === 1)
    // results should add to row_count
    assert(finalDf.where("sex == 'male'").count() === 2)
    assert(finalDf.where("sex == 'female'").count() === 3)
    // each row has the same value, so the count of them should be row_count
    assert(finalDf.where("primaryNeeds == 1.5").count() === row_count)
    assert(finalDf.where("work == 2.0").count() === row_count)
    assert(finalDf.where("other == 7.0").count() === row_count)
  }

  // @see https://stackoverflow.com/questions/31197353/dataframe-equality-in-apache-spark
  private def assertSmallDataFrameEquality(actualDf: DataFrame,
                                           expectedDf: DataFrame,
                                           includeSchema: Boolean = true): Unit =
  {
    if (includeSchema)
      assert(actualDf.schema.equals(expectedDf.schema), "DataFrame schemas don't match")
    assert(actualDf.collect().sameElements(expectedDf.collect()), "DataFrame contents don't match")
  }

  test("timeUsageGroupedSql functionality") {
    val fromSql = timeUsageGroupedSql(summaryDf).cache()
    //fromSql.show()

    assertSmallDataFrameEquality(fromSql, finalDf)
  }

  val typedSummaryDS = timeUsageSummaryTyped(summaryDf).cache()

  test("timeUsageSummaryTyped functionality") {
    //typedSummaryDS.show()

    assertSmallDataFrameEquality(typedSummaryDS.toDF(), summaryDf)
  }

  test("timeUsageGroupTyped functionality") {
    val typed = timeUsageGroupedTyped(typedSummaryDS)

    assertSmallDataFrameEquality(typed.toDF, finalDf)
  }

}
