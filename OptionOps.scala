object Default {

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  /**
    * @return All primitive types return zero value. String returns empty string.
    *         All other reference type returns null
    */
  def default[T]: T = macro defaultImpl[T]

  def defaultImpl[T : c.WeakTypeTag](c: Context): c.Expr[T] = {
    import c.universe._
    c.weakTypeOf[T] match {
      case t if t == c.weakTypeOf[Int] => c.Expr[T](Literal(Constant(0)))
      case t if t == c.weakTypeOf[Long] => c.Expr[T](Literal(Constant(0l)))
      case t if t == c.weakTypeOf[Char] => c.Expr[T](Literal(Constant('\u0000')))
      case t if t == c.weakTypeOf[Float] => c.Expr[T](Literal(Constant(0f)))
      case t if t == c.weakTypeOf[Double] => c.Expr[T](Literal(Constant(0d)))
      case t if t == c.weakTypeOf[Boolean] => c.Expr[T](Literal(Constant(false)))
      case t if t == c.weakTypeOf[Unit] => c.Expr[T](Literal(Constant(())))
      case t if t <:< c.weakTypeOf[AnyVal] => c.Expr[T](Literal(Constant(0)))
      case t if t == c.weakTypeOf[String] => c.Expr[T](Literal(Constant("")))
      case _ => c.Expr[T](Literal(Constant(null.asInstanceOf[T])))
    }
  }

//  trait HasDefault[@specialized(Specializable.Primitives) T] {
//    @inline
//    def default: T
//  }
//
//  implicit object IntHasDefault extends HasDefault[Int] { @inline val default = 0 }
//  implicit object LongHasDefault extends HasDefault[Long] { @inline val default = 0L }
//  implicit object CharHasDefault extends HasDefault[Char] { @inline val default = '\u0000'}
//  implicit object FloatHasDefault extends HasDefault[Float] { @inline val default = 0f }
//  implicit object DoubleHasDefault extends HasDefault[Double] { @inline val default = 0d }
//  implicit def RefHasDefault[T]: HasDefault[T] = new HasDefault[T] { @inline def default: T = null.asInstanceOf[T] }
//
//  @inline
//  def default[@specialized(Specializable.Primitives) T](implicit ev: HasDefault[T]) = ev.default

}


object OptionOps {

  implicit class OptionOps[T](p: Option[T]) {
    import hb.util.Default._
    import hb.util.function._

    // Use these traits to avoid boxing due to scala.Function1 cannot be partially specialized
    def getField(f: ToIntFunction[T]): Int = if (p.isDefined) f(p.get) else default[Int]
    def getField(f: ToDoubleFunction[T]): Double = if (p.isDefined) f(p.get) else default[Double]
    def getField(f: ToLongFunction[T]): Long = if (p.isDefined) f(p.get) else default[Long]
    def getField(f: ToCharFunction[T]): Char = if (p.isDefined) f(p.get) else default[Char]
    def getField(f: ToShortFunction[T]): Short = if (p.isDefined) f(p.get) else default[Short]
    def getField(f: ToUnitFunction[T]): Unit = if (p.isDefined) f(p.get) else default[Unit]
    def getField(f: ToFloatFunction[T]): Float = if (p.isDefined) f(p.get) else default[Float]
    def getField(f: ToByteFunction[T]): Byte = if (p.isDefined) f(p.get) else default[Byte]
    def getField(f: ToBooleanFunction[T]): Boolean = if (p.isDefined) f(p.get) else default[Boolean]
    def getField(f: T => String): String = if (p.isDefined) f(p.get) else default[String]
//    def getField[R >: Null <: AnyRef](f: T => R): R = p.map(f).getOrElse(default[R])

  }

}

object OptionOpsGen {

  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  object OptionOpsImpl {
    def getFieldImpl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(f: c.Tree) = {
      import c.universe._

      val o = c.prefix

      val q = c.weakTypeOf[R] match {
        case t if t <:< c.weakTypeOf[Int] =>
//          val castedF = (reify {
//            f.splice.asInstanceOf[java.util.function.ToIntFunction[R]]
//          })
          println(f)
//          val cf  = Typed(Function(List(f.children(0).asInstanceOf[ValDef]), f.children(1)), AppliedTypeTree(
//            Ident(TermName("java.util.function.ToIntFunction")), List(Select(
//              Ident(TermName("Predef")), TermName("String"))
//            )
//          ))
//          println(cf)


          q"if ($o.value.isDefined) (${f}: java.util.function.ToIntFunction[$t]).applyAsInt($o.value.get) else 0"
        case _ =>
          q"if ($o.value.isDefined) ($f)($o.value.get) else null"
      }

      println(q)
      q
//      c.Expr(q)
    }
  }

  implicit class OptionOps[T](val value: Option[T]) {

    def getField[R](f: T => R): R = macro OptionOpsImpl.getFieldImpl[T, R]

  }
  //Some("1").getField(_.toInt)

}
