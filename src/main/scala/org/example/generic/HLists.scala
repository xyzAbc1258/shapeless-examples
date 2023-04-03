package org.example.generic

import io.circe.{Decoder, Encoder, JsonObject}
import shapeless._
import shapeless.labelled.FieldType
import shapeless.tag.@@

object HLists extends App {
  case class Sample(id: Int, name: String, age: Option[Int])

  //Generic is a simple transformation between type and it's representation
  //HList is just a list which holds information about types of all its elements
  val generic: Generic.Aux[Sample, Int :: String :: Option[Int] :: HNil] = Generic.materialize

  println("Generic to: " + generic.to(Sample(1, "Abc", Some(26))))

  println("Generic from: " + generic.from(1 :: "Abc" :: Some(26) :: HNil))

  //LabelledGeneric enriches representation with additional type information.
  //This information can only be used in compilation time!
  val labelledGeneric: LabelledGeneric.Aux[
    Sample,
    FieldType[Symbol @@ "id", Int] ::
      FieldType[Symbol @@ "name", String] ::
      FieldType[Symbol @@ "age", Option[Int]] ::
      HNil
  ] =
    the[LabelledGeneric[Sample]]

  println("Labeledd generic: " + labelledGeneric.to(Sample(1, "Abc", Some(26))))

  //We can use LabelledGeneric and it's metadata to build encoder
  object GenEncoder {

    implicit def encoderForHNil: Encoder.AsObject[HNil] = _ => JsonObject.empty //base case

    //And recursion over shape
    implicit def encodeCons[K <: Symbol, V: Encoder, T <: HList: Encoder.AsObject](implicit
        witnessK: Witness.Aux[K]
    ): Encoder.AsObject[FieldType[K, V] :: T] =
      (x: FieldType[K, V] :: T) => {
        val tailObject = Encoder.AsObject[T].encodeObject(x.tail)
        tailObject.+:(witnessK.value.name -> Encoder[V].apply(x.head))
      }

    //Helper wrapping conversion.
    //Order of arguments is important. Since A will be deduced at the place of invocation,
    //we need LabelledGeneric to deduce H, and only later we can try to build Encoder.AsObject to H.
    //Placing Encoder.AsObject as a constraint (H <: HList: Encoder.AsObject) won't work
    def encoderForLabelled[A, H <: HList](implicit
        g: shapeless.LabelledGeneric.Aux[A, H],
        encoder: Encoder.AsObject[H]
    ): Encoder.AsObject[A] = (x: A) => encoder.encodeObject(g.to(x))
  }

  val sampleEncoder: Encoder.AsObject[Sample] = {
    import GenEncoder._
    encoderForLabelled
  }
  println(
    "LabelledGeneric encoder: " + io.circe.Printer.spaces2
      .print(sampleEncoder(Sample(1, "Abc", Some(26))))
  )

  //Similarly we can define decoder
  object GenDecoder {
    implicit val hnilDecoder: Decoder[HNil] = Decoder(_ => Right(HNil)) //base case
    //Recursion over hlist shape
    implicit def ftDecoder[K <: Symbol, V: Decoder, T <: HList: Decoder](implicit
        w: Witness.Aux[K]
    ): Decoder[FieldType[K, V] :: T] = Decoder(h => {
      for {
        v <- h.downField(w.value.name).as[V]
        t <- h.as[T]
      } yield labelled.field[K](v) :: t
    })

    def gen[A, H <: HList](implicit g: LabelledGeneric.Aux[A, H], d: Decoder[H]): Decoder[A] = d.map(g.from)
  }
}
