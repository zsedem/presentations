package com.hiya.shapeless.demo

import shapeless.Coproduct.unsafeGet
import shapeless.labelled.FieldType
import shapeless.{LabelledGeneric, _}

trait Diffable[T] {
  def diff(a: T)(b: T): DiffResult
}

sealed trait DiffResult
case class CompleteDiff(a: String, b: String) extends DiffResult
case class PartialDiff(fieldDiffs: List[FieldDiff]) extends DiffResult {
  override def toString: String = fieldDiffs.map(_.toString).mkString("\n")
}
case class FieldDiff(path: String, diff: DiffResult) {
  override def toString: String =
    s"""$path:
       |-${diff.asInstanceOf[CompleteDiff].a}
       |+${diff.asInstanceOf[CompleteDiff].b}
       |""".stripMargin
}
case object NoDiff extends DiffResult {
  override def toString: String = ""
}

object Diffable extends PrimitiveInstances {
  def apply[T](implicit diffable: Diffable[T]): Diffable[T] = diffable

  implicit def diffableFromGenericHList[T, GenericT <: HList](implicit
                                                         generic: LabelledGeneric.Aux[T, GenericT],
                                                         diffableForGeneric: Lazy[Diffable[GenericT]]
                                                         ): DerivedDiffable[T] = {
    new Diffable[T] with DerivedTag {
      override def diff(a: T)(b: T): DiffResult = {
        diffableForGeneric.value.diff(generic.to(a))(generic.to(b))
      }
    }
  }

  implicit def diffableFromGenericCoproduct[T, GenericT <: Coproduct](implicit
                                                              generic: LabelledGeneric.Aux[T, GenericT],
                                                              diffableForGeneric: Lazy[Diffable[GenericT]]
                                                             ): DerivedDiffable[T] = {
    new Diffable[T] with DerivedTag {
      override def diff(a: T)(b: T): DiffResult = {
        diffableForGeneric.value.diff(generic.to(a))(generic.to(b))
      }
    }
  }

  trait DerivedTag
  type DerivedDiffable[T] = Diffable[T] with DerivedTag


  implicit def encodeHList[K <: Symbol, H, Tail <: HList](
                                                           implicit eh: Diffable[H],
                                                           et: Lazy[Diffable[Tail]],
                                                           k: Witness.Aux[K]): DerivedDiffable[FieldType[K, H] :: Tail] =
    new Diffable[FieldType[K, H] :: Tail] with DerivedTag {
      def diff(a: FieldType[K, H] :: Tail)(b: FieldType[K, H] :: Tail): DiffResult = {
        eh.diff(a.head)(b.head) match {
          case NoDiff => et.value.diff(a.tail)(b.tail)
          case diff@PartialDiff(_) =>
            val diffOfThisField = diff.fieldDiffs.map(x => x.copy(path = s"${k.value.name}.${x.path}"))
            PartialDiff(et.value.diff(a.tail)(b.tail) match {
              case PartialDiff(fieldDiffs) => diffOfThisField ++ fieldDiffs
              case NoDiff => diffOfThisField
              case CompleteDiff(_, _) => sys.error("Impossible happened")
            })
          case diff: CompleteDiff =>
            val diffOfThisField = FieldDiff(k.value.name, diff)
            PartialDiff(et.value.diff(a.tail)(b.tail) match {
              case PartialDiff(fieldDiffs) => diffOfThisField +: fieldDiffs
              case NoDiff => List(diffOfThisField)
              case CompleteDiff(_, _) => sys.error("Impossible happened")
            })
        }
      }

    }

  implicit def encodeCoProduct[K <: Symbol, H, Tail <: Coproduct](
                                                           implicit eh: Diffable[H],
                                                           et: Lazy[Diffable[Tail]]
                                                                 ): DerivedDiffable[FieldType[K, H] :+: Tail] =
    new Diffable[FieldType[K, H] :+: Tail] with DerivedTag {
      def diff(a: FieldType[K, H] :+: Tail)(b: FieldType[K, H] :+: Tail): DiffResult = {
        (a.head, b.head) match {
          case (Some(a1), Some(b1)) =>
            eh.diff(a1)(b1)
          case (None, None) =>
            et.value.diff(a.tail.get)(b.tail.get)
          case (_, _) =>
            CompleteDiff(unsafeGet(a).toString, unsafeGet(b).toString)
        }
      }

    }


  implicit val diffableForEmptyGeneric: DerivedDiffable[HNil] = {
    new Diffable[HNil] with DerivedTag {
      override def diff(a: HNil)(b: HNil): DiffResult = NoDiff
    }
  }

  implicit val diffableForEmptyGenericCoproduct: DerivedDiffable[CNil] = {
    new Diffable[CNil] with DerivedTag {
      override def diff(a: CNil)(b: CNil): DiffResult = NoDiff
    }
  }
}
trait PrimitiveInstances {
  implicit val diffableInt: Diffable[Int] = new Diffable[Int] {
    override def diff(a: Int)(b: Int): DiffResult = if (a == b) {
      NoDiff
    } else {
      CompleteDiff(a.toString, b.toString)
    }
  }
  implicit val diffableString: Diffable[String] = new Diffable[String] {
    override def diff(a: String)(b: String): DiffResult = if (a == b) {
      NoDiff
    } else {
      CompleteDiff(a, b)
    }
  }
}

case class TestCaseClass(a: String, b: SubField, d: Direction)
case class SubField(c: Int)
sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

object DiffableMain extends App {
  private val data: TestCaseClass = TestCaseClass("a", SubField(15), Up)
  private def tryD(f: TestCaseClass => TestCaseClass) = {
    Diffable[TestCaseClass].diff(data)(f(data))
  }

  val generic = Generic[Direction]
  generic.to(Up)
  println()
  println(tryD(x => x))
  println()

  println()
  println(tryD(x => x.copy(a = "z")))
  println()


  println()
  println(tryD(x => x.copy(b = SubField(42))))
  println()


  println()
  println(tryD(x => x.copy(d = Down)))
  println()
}
