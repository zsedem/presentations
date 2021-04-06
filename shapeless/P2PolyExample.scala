package com.hiya.shapeless.demo

import shapeless.Poly2

sealed trait Event

case class CallEvent(phone: String) extends Event
case class TextEvent(phone: String) extends Event

object Event extends Poly2 {
  sealed trait EventType
  case object Call extends EventType
  case object Text extends EventType
  implicit val toCallEvent: Case.Aux[Call.type, String, CallEvent] = {
    at.apply((_, x: String) => {
      CallEvent(x)
    })
  }
  implicit val toTextEvent: Case.Aux[Text.type, String, TextEvent] = {
    at.apply((_, x: String) => {
      TextEvent(x)
    })
  }

  implicit val toEvent: Case.Aux[EventType, String, Event] = {
    at.apply({
      case (Text, x: String) => TextEvent(x)
      case (Call, x: String) => CallEvent(x)
    })
  }
}

class P2PolyExample {
  import Event._
  val callEvent: CallEvent = Event(Call, "1/112")

  val textEvent: TextEvent = Event(Text, "1/112")
  val mixedEvents1: List[Event] = List(callEvent, textEvent)
  val mixedEvents2: List[Event] = List[EventType](Call, Text).map(Event(_, "1/112"))
}
