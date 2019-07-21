import scala.language.implicitConversions

class LazyVar[T](value: => T) {
  private[this] var _value: T = _
  private[this] var gen: () => T = () => value
  @volatile private[this] var genCalled: Boolean = false

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
  
  def get: T = apply()
  
  def compareAndSet(v: T, u: => T): Boolean = {
    if (this.get == v) {
        this.synchronized {
            if(this.get == v) {
                this.update(u)
                true
            } else false
        }
    }
    else false
  }

}

object LazyVar {
  def apply[T](value: => T) = new LazyVar[T](value)

  implicit def getValue[T](lv: LazyVar[T]): T = lv()
}

//Test&Example
object Test extends App {
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
