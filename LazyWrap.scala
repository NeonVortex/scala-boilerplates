import scala.language.implicitConversions

class LazyWrap[T](value: => T) {
  def apply(): T = value
}

object LazyWrap {
  def apply[T](value: => T) = new LazyWrap[T](value)

  implicit def toValue[T](lw: LazyWrap[T]): T = lw()
}

//Usage
object Foo {
  val foo = "foo"
  var bar: LazyWrap[String] = LazyWrap {
    println("evaluated")
    "bar"
  }
}

Foo.foo
Foo.bar
