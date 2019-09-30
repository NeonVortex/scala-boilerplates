import scala.collection.mutable

class CoalescingQueue[T](coalesceBy: T => Any, maxLength: Int = Int.MaxValue) {
  val m = mutable.HashMap[Any, T]()
  val q = mutable.Queue[T]()

  def enqueue(v: T): Option[T] = {
    val prev = m.put(coalesceBy(v), v)
    if (q.length >= maxLength) {
      if (prev.isEmpty) {
        q.dequeue()
      }
      else {
        q.dequeueFirst(prev.contains)
      }
    }
    q.enqueue(v)
    prev
  }

  def dequeue: Option[T] = {
    var result: Option[T] = None
    var found: Boolean = false
    while (q.nonEmpty && !found) {
      val v = q.dequeue()
      val k = coalesceBy(v)
      if (m(k) == v) {
        m.remove(k)
        result = Some(v)
        found = true
      }
    }
    result
//    for (first <- value.headOption) yield {
//      value.remove(first._1)
//      first._2
//    }
  }
}
