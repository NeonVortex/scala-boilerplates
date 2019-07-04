import java.util.concurrent.atomic.AtomicReference

class LazyAtomicReference[T](value: T = null) extends AtomicReference(value) {
    
    def compareAndSet(v: T, u: () => T): Boolean =
        compareAndSetLazy(v, u.apply)

    def compareAndSetLazy(v: T, u: => T): Boolean = {
        if (this.get == v) {
            this.synchronized {
                if(this.get == v) {
                    this.set(u)
                    //value = u
                    true
                } else false
            }
        }
        else false
    }
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
