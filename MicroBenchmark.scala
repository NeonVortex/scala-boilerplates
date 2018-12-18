import scala.concurrent.duration._
import scala.collection.mutable

object ListBM extends App {
private def measureAvgExecTime[T](n: Int)(op: => T): Duration = {
  def measureExecTime[T]: Long = {
    val startTime = System.nanoTime()
    op
    val endTime = System.nanoTime()
    (endTime - startTime)
  }

  val values: Seq[Long] = (1 until n).map(_ => measureExecTime)
  values.sum / n nanoseconds
}

val i = 1000
val r = 1 to 1000

val it1 = measureAvgExecTime(i) {
  var l = List.empty[Int]
  r.foldLeft(l) { (o, i) =>
    i :: o
  }
}

val it1_2 = measureAvgExecTime(i) {
  var l = List.empty[Int]
  r.foldLeft(l) { (o, i) =>
    i :: o
  }
}


val it1_3 = measureAvgExecTime(i) {
  var l = List.empty[Int]
  r.foldLeft(l) { (o, i) =>
    o :+ i
  }
}

val it2 = measureAvgExecTime(i) {
  val l = mutable.MutableList.empty[Int]
  r.foreach { i =>
    l += i
  }
}


println(s"List (prepend): ${it1}")
println(s"List (warm prepend): ${it1_2}")
println(s"List (append): ${it1_3}")
println(s"MutableList: ${it2}")


}
