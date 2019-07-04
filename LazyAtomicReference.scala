class LazyAtomicReference[T](@volatile var value: T = null) {
    def compareAndSet(v: => T, u: => T) = {
        if (value == v) {
            synchronized {
                if(value == v) {
                    value = u
                    true
                } else false
            }
        }
        else false
    }

    def get = value
}

//Test
class Foo {
    val s = new LazyAtomicReference[String]()

    def foo(): String = {
        println("called")
        "foo"
    }

    def get() = {
        s.compareAndSet(null, foo())
        s.get
    }
}

import scala.concurrent._
import scala.concurrent.duration.Duration

val f = new Foo
implicit val ec = scala.concurrent.ExecutionContext.global
(1 to 100) foreach { _ =>
    Future { assert(f.get() != null) }
}
Await.result(Future.never, Duration.Inf)
