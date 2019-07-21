import scala.language.implicitConversions

class LazyVar[T] {
  @volatile private[this] var _value: T = _
  @volatile private[this] var gen: () => T = _
  @volatile private[this] var genCalled: Boolean = true
  
  def this(value: => T) = {
    this()
    gen = () => value
    genCalled = false
  }

  def apply(): T = {
    if (!genCalled) {
      synchronized {
        if (!genCalled) {
          _value = gen()
          genCalled = true
        }
      }
    }

    _value
  }
  
  def update(newValue: => T): Unit = synchronized {
    gen = () => newValue
    genCalled = false
  }
  
  def compareAndSet(v: T, u: => T): Boolean = synchronized {
    if (this.apply() == v) {
      this.update(u)
      true
    }
    else false
  }

}

object LazyVar {
  def apply[T]() = new LazyVar[T]
  def apply[T](value: => T) = new LazyVar[T](value)

  implicit def getValue[T](lv: LazyVar[T]): T = lv()
}

//Test&Example
object Test1 {
  trait Foo {
    def foo: String
    //val foobar = foo + "bar" //This throws UninitializedFieldError with -xcheckinit or nullbar if no check
    val foobar = LazyVar[String](foo + "bar")
  }
  
  class FooImpl extends Foo {
    val foo = "foo"
    //println(foobar)
    println(foobar())
    //To update foobar, use:
    //foobar() = "foobar2"
  }
  
  new FooImpl()  //print foobar; without LazyVar, this will print nullbar
}
object Test2 {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  import scala.concurrent.Future
  
  val f = new LazyVar[String]
  def foo = {
    println("should be called only once")
    "foo"
  }
  
  (1 to 10).foreach{_=>Future{
    f.compareAndSet(null, foo) //print "should be called only once"
  }}
  
  println(f())  //print "foo"
}
