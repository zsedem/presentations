package com.hiya.shapeless.demo

import shapeless.{LowPriority, Poly1}

sealed trait JsValue
case object JsNull extends JsValue
case class JsString(str: String) extends JsValue
case class JsArray(array: Array[JsValue]) extends JsValue









































object toJson extends Poly1 {
  implicit val toJsonString: Case.Aux[String, JsString] = {
    at.apply((x: String) => { JsString(x) })
  }

  implicit val toJsonNull: Case.Aux[None.type, JsNull.type] = {
    at.apply(_ => JsNull)
  }

  implicit def toJsonSomeT[T, Result <: JsValue](implicit aux: Case.Aux[T, Result]): Case.Aux[Some[T], Result] = {
    at.apply({ case Some(x) => aux.apply(x) })
  }

  implicit def toOptionT[T, Result <: JsValue](implicit aux: Case.Aux[T, Result], lowPrio: LowPriority): Case.Aux[Option[T], JsValue] = {
    at.apply({
      case Some(x) => aux.apply(x)
      case None => JsNull
    })
  }

  implicit def toJsonArray[T, Result <: JsValue](implicit aux: Case.Aux[T, Result]): Case.Aux[List[T], JsArray] = {
    at.apply((list: List[T]) => JsArray(list.map(x => aux.apply(x)).toArray))
  }
}

object P1Main extends App {
  val js1: JsString = toJson[String]("hello")
  val js2: JsNull.type = toJson[None.type](None)
  val js3: JsString = toJson[Some[String]](Some("hello"))
  val js4: JsValue = toJson[Option[String]](Option("hello"))

  val js5: JsArray = toJson[List[String]](List("1", "2", "3"))
}
