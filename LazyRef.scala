import scala.language.implicitConversions

class LazyRef[T](value: => T) {
  lazy val _value = value
  def apply(): T = _value
}

object LazyRef {
  def apply[T](value: => T) = new LazyRef[T](value)

  implicit def toValue[T](lw: LazyRef[T]): T = lw()
}

//Usage
object Foo {
  val foo = "foo"
  var bar: LazyRef[String] = LazyRef {
    println("evaluated")
    "bar"
  }
}

println(Foo.foo)
println(Foo.bar())
