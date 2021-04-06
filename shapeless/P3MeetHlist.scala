package com.hiya.shapeless.demo ///

import com.hiya.shapeless.demo.ToString.ops
import shapeless._
import simulacrum.typeclass

import scala.reflect.ClassTag


case class Y(
  str1: String,
  int: Int
)

case class X(
  str2: String,
  int: Int
)






















object P3MeetHlist extends App {
  val generic = Generic[X] ///
  type HListRepr = generic.Repr

  val evidence = implicitly[HListRepr =:= (String :: Int :: HNil)]

  assert(generic.to(X("hello", 42)) == ("hello" :: 42 :: HNil))































  def f(x: String :: Int :: HNil): String = {
    s"${x.head} :: ${x.tail.head} :: HNil zsiga" ///
  }

//   f(X("hello", 42)) // does not compile

































  def f2[T](t: T)(implicit generic: Generic.Aux[T, String :: Int :: HNil]): String = {
    f(generic.to(t)) ///
  }

  println(f2(X("hello", 42)))
  println(f2(Y("hello", 42)))
}






























object Main extends App {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  // X -> String :: Int :: HNil -> Y
  def conversion[From, To, GenericT](t: From)(
    implicit fromGeneric: Aux[From, GenericT],                                        ///
              toGeneric: Aux[To, GenericT]): To = {
    toGeneric.from(fromGeneric.to(t))
  }

  println(conversion[X, Y, String :: Int :: HNil](X("hello", 42))(fromGeneric = Generic[X], toGeneric = Generic[Y]))

}





























@typeclass
trait ToString[T] {
  def toStr(t: T): String
}
object ToString extends LowPrioInstances {
  private def forAny[T](implicit ct: ClassTag[T]): ToString[T] = new DerivedToString[T] {
    override def toStr(t: T): String = s"${ct.runtimeClass.getSimpleName}(${t.toString})" // Int(42)
  }
  implicit val intToString: ToString[Int] = forAny
  implicit val stringToString: ToString[String] = forAny
  implicit val doubleToString: ToString[Double] = forAny
  implicit def optionToString[T]: ToString[Option[T]] = forAny
}

trait LowPrioInstances {
  trait DerivedToString[T] extends ToString[T]
  implicit val hNilInstance: DerivedToString[HNil] = new DerivedToString[HNil] {
    override def toStr(t: HNil): String = "HNil"
  }

  implicit def hListIteration[Head, Tail <: HList](
    implicit toStringHead: ToString[Head],
    toStringTail: DerivedToString[Tail] // hListIteration | hNilInstance
  ): DerivedToString[Head :: Tail] = new DerivedToString[Head :: Tail] {
    override def toStr(t: Head :: Tail): String =
      s"${ToString[Head](toStringHead).toStr(t.head)} :: ${ToString[Tail](toStringTail).toStr(t.tail)}"
  }


  implicit def gen[GenericT, T](
                                 implicit generic: Generic.Aux[T, GenericT],
                                 derivedToString: DerivedToString[GenericT]
  ): ToString[T] = new DerivedToString[T] {
    override def toStr(t: T): String = derivedToString.toStr(generic.to(t))
  }

}

object TestToString extends App {
  private val typeClassInstance: ToString[X] = ToString[X](
    ToString.gen[String :: Int :: HNil, X](
      Generic[X],
      ToString.hListIteration(
        ToString.stringToString,
        toStringTail = ToString.hListIteration(
          ToString.intToString,
          ToString.hNilInstance
        )
      )
    )
  )
  println(
    typeClassInstance.toStr(X("str", 10))
  )

  case class Z(a: String, b: Int)
  println(
    ToString[Z].toStr(Z("fdsf", 32))
  )
}







































