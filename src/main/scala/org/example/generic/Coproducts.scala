package org.example.generic

import cats.implicits.toFunctorOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import io.circe.syntax.EncoderOps
import shapeless._
import shapeless.labelled.FieldType
import shapeless.tag.@@

object Coproducts extends App {
  sealed trait Person
  case class Student(name: String, indexNumber: String) extends Person
  object Student {
    implicit val studentEncoder: Encoder.AsObject[Student] = {
      import HLists.GenEncoder._
      encoderForLabelled
    }
    implicit val studentDecoder: Decoder[Student] = {
      import HLists.GenDecoder._
      gen
    }
  }
  case class Professor(name: String, employeeNumber: String) extends Person
  object Professor {
    implicit val professorEncoder: Encoder.AsObject[Professor] = {
      import HLists.GenEncoder._
      encoderForLabelled
    }
    implicit val professorDecoder: Decoder[Professor] = {
      import HLists.GenDecoder._
      gen
    }
  }

  //Generic representation of interface as a variant of all it's implementations
  //Coproduct is just an generalized either with Inr and Inl constructors
  //where Inl holds real value, and Inr is used to nest value deeper
  val generic: Generic.Aux[Person, Professor :+: Student :+: CNil] =
    Generic.materialize

  println("Student: " + generic.to(Student("Abc", "Index number")))
  println("Professor: " + generic.to(Professor("Abc", "Employee number")))
  println(
    "Coproduct back: " + generic.from(
      //Automatically places value inside coproduct
      Coproduct[Professor :+: Student :+: CNil](
        Student("Abc", "Another index")
      )
    )
  )

  println(
    "Coproduct back 2: " + generic
      .from(Inr(Inl(Student("Abc", "Another index"))))
  )

  //Labelled version holds information about implementation type names
  val labeleddGeneric: shapeless.LabelledGeneric.Aux[
    Person,
    FieldType[Symbol @@ "Professor", Professor] :+:
      FieldType[Symbol @@ "Student", Student] :+:
      CNil
  ] = the[LabelledGeneric[Person]]
  //the is a better implicitly. Implicitly can lose information about internal type aliases, the doesn't
  //In this place implicitly wouldn't work, but something like below would be okay
  ////def myThe[A](implicit a: A): a.type = a

  //Again we can recurse over shape of Coproduct to derive structures.
  object SimpleEncoder {
    implicit def encoderForCNil: Encoder[CNil] = (x: CNil) => x.impossible

    implicit def encoderForCCons[H: Encoder, T <: Coproduct: Encoder]: Encoder[H :+: T] = (x: H :+: T) =>
      x.eliminate(_.asJson, _.asJson)

    def genericEncoder[A, C <: Coproduct](implicit g: Generic.Aux[A, C], encoder: Encoder[C]): Encoder[A] = (x: A) =>
      encoder(g.to(x))

  }

  private val personEncoder: Encoder[Person] = {
    import SimpleEncoder._
    genericEncoder
  }

  println("PersonEncoder from generic: " + io.circe.Printer.spaces2.print(personEncoder(Student("Student", "Index"))))
  //Again we can use additional annotation from LabeleedGeneric
  object AsObject {
    implicit def encoderForCNil: Encoder.AsObject[CNil] = (c: CNil) => c.impossible

    implicit def encoderForCCons[K <: Symbol, H: Encoder, T <: Coproduct: Encoder.AsObject](implicit
        witness: Witness.Aux[K]
    ): Encoder.AsObject[FieldType[K, H] :+: T] = (x: FieldType[K, H] :+: T) =>
      x.eliminate(
        x => JsonObject(witness.value.name -> (x: H).asJson),
        t => Encoder.AsObject[T].encodeObject(t)
      )

    def encodeWithName[A, C <: Coproduct](implicit
        g: LabelledGeneric.Aux[A, C],
        encoder: Encoder.AsObject[C]
    ): Encoder.AsObject[A] = (x: A) => encoder.encodeObject(g.to(x))
  }

  val labelledPersonEncoder: Encoder.AsObject[Person] = {
    import AsObject._
    encodeWithName
  }

  println(
    "LabelledPersonEncoder from generic: " + io.circe.Printer.spaces2
      .print(labelledPersonEncoder(Professor("Prof", "Employee number")))
  )

  object Decoder {
    //Helper type
    case class Decoders[A, C <: Coproduct](d: Decoder[A])
    object Decoders { //Again recursion over shape
      implicit def decoderForSingle[A, H <: A](implicit d: Decoder[H]): Decoders[A, H :+: CNil] = Decoders(d.widen)
      implicit def decoderForCons[A, H <: A, T <: Coproduct](implicit
          hd: Decoder[H],
          decsT: Decoders[A, T]
      ): Decoders[A, H :+: T] =
        Decoders(hd.widen[A].or(decsT.d))
    }

    def genericDecoder[A, C <: Coproduct](implicit
        g: Generic.Aux[A, C],
        d: Decoders[A, C]
    ): Decoder[A] = d.d
  }

  val personDecoder: Decoder[Person] = Decoder.genericDecoder

  println(
    "Decoded professor: " + personDecoder.decodeJson(
      Json.fromJsonObject(
        JsonObject(
          "name" -> "Prof".asJson,
          "employeeNumber" -> "Empl number".asJson
        )
      )
    )
  )

}
