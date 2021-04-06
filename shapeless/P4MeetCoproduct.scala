package com.hiya.shapeless.demo
import shapeless._


sealed trait CoproductExample
case class A(a: String) extends CoproductExample
case class B(b: String) extends CoproductExample

object P4MeetCoproduct extends App {
  type CoproductExampleGenericT = A :+: B :+: CNil

  val generic = Generic[CoproductExample]
  type R = generic.Repr
  implicitly[R =:= CoproductExampleGenericT]

  val a: CoproductExampleGenericT = Inl(A("a"))
  val b: CoproductExampleGenericT = Inr(Inl(B("a")))
  List(a, b)

  def f(error: String :+: Throwable :+: CNil): String = {
    val _: Option[String] = error.head
    val _: Option[Throwable] = error.tail.map(_.unify)
    val _: Option[Throwable] = error.select[Throwable]
    error.eliminate[String](
      x => s"error ${x}",
      x => s"exception: ${x.unify.getMessage}\n ${x.unify.getStackTrace.mkString("    \n")}"
    )
  }
  println(f(Inl("string error")))
  println(f(Inr(Inl(new RuntimeException("exc")))))

}
