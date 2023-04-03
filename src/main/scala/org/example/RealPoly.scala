package org.example

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import org.example.generic.Coproducts.Professor
import org.example.generic.HLists.Sample
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Mapped

object RealPoly extends App {
  object Implicit extends Poly0 {
    implicit def get[A](implicit a: A) = at[A](a)
  }

  object GenDecoderOnceMore {
    object DecodeField extends Poly0 {
      //Decoder for Field[K, V] which looks under field K, and decodes V
      implicit def decodeField[K <: Symbol, V: Decoder](implicit w: Witness.Aux[K]): Case0[Decoder[FieldType[K, V]]] = {
        at[Decoder[FieldType[K, V]]](Decoder(_.get[V](w.value.name)).map(labelled.field[K](_)))
      }
    }

    object FoldDecoders extends Poly2 {
      implicit def fold[A, B <: HList]: Case.Aux[Decoder[A], Decoder[B], Decoder[A :: B]] =
        at[Decoder[A], Decoder[B]]((ad, bd) =>
          Decoder(h =>
            for {
              a <- ad(h)
              b <- bd(h)
            } yield a :: b
          )
        )
    }

    //A is represented as hlist H
    //HD is a hlist of decoders of elems of H
    //Create instance for each elem of HD
    //Fold decoders to receive Decoder[H]
    def genDecoder[A, H <: HList, HD <: HList](implicit
        g: shapeless.LabelledGeneric.Aux[A, H],
        mappedD: Mapped.Aux[H, Decoder, HD],
        fillWith: ops.hlist.FillWith[DecodeField.type, HD],
        folds: ops.hlist.RightFolder.Aux[HD, Decoder[HNil], FoldDecoders.type, Decoder[H]]
    ): Decoder[A] = {
      val decodeHNil: Decoder[HNil] = Decoder(_ => Right(HNil))
      HList.fillWith[HD](DecodeField).foldRight(decodeHNil)(FoldDecoders).map(g.from)
    }
  }

  val hlistSampleDecoder: Decoder[generic.HLists.Sample] = GenDecoderOnceMore.genDecoder
  println(
    "LabeleddDecoder: " + hlistSampleDecoder.decodeJson(
      Json.fromJsonObject(
        JsonObject(
          "id" -> 12.asJson,
          "name" -> "abc".asJson
        )
      )
    )
  )

  object GenericEncodersOnceMore {
    trait LowPrioEncode extends Poly1 {
      implicit def encodeSimple[A: Encoder] = at[A](_.asJson)
    }
    object Encode extends LowPrioEncode {
      implicit def encodeField[K <: Symbol, V: Encoder](implicit w: Witness.Aux[K]) =
        at[FieldType[K, V]](x => JsonObject(w.value.name -> (x: V).asJson))
    }

    //A is represented as coproduct C
    //Folder is really a mapper and unifier
    // We map each elem of C to Json, using encoders implicitly
    // We unify coproduct consisting of only Json
    def genEncodeCoproduct[A, C <: Coproduct](implicit
        g: Generic.Aux[A, C],
        fold: ops.coproduct.Folder.Aux[Encode.type, C, Json]
    ): Encoder[A] = (x: A) => g.to(x).fold(Encode)

    //A is represented as a hlist H consisting of Field[FieldName, Value]
    //We encode each field into it's own JsonObject, receiving hlist M consisting only of JsonObjects
    //We collect M into List[JsonObject]
    def genEncodeHList[A, H <: HList, M <: HList](implicit
        g: LabelledGeneric.Aux[A, H],
        mapper: ops.hlist.Mapper.Aux[Encode.type, H, M],
        toList: ops.hlist.ToTraversable.Aux[M, List, JsonObject]
    ): Encoder[A] = (x: A) =>
      g.to(x).map(Encode).toList.reduceLeftOption(_ deepMerge _).map(Json.fromJsonObject).getOrElse(Json.Null)

    object DecoderOr extends Poly2 {
      // _ or _
      //Lub is a proof that A <: C and B <: C
      implicit def or[A, B, C](implicit lub: Lub[A, B, C]) =
        at[Decoder[A], Decoder[B]](_.map(lub.left) or _.map(lub.right))
    }

    //A is represented as a coproduct C
    //All types in C we collect in hlist H
    //We build TYPE hlist HD consisting of decoders of H
    //We build VALUE HD by collection implicit values
    //We fold list HD, by calling or on decoders,
    def genCoproductDecoder[A, C <: Coproduct, H <: HList, HD <: HList](implicit
        g: Generic.Aux[A, C],
        toHlist: ops.coproduct.ToHList.Aux[C, H],
        hdProof: ops.hlist.Mapped.Aux[H, Decoder, HD],
        hdFill: ops.hlist.FillWith[Implicit.type, HD],
        fold: ops.hlist.LeftFolder.Aux[HD, Decoder[A], DecoderOr.type, Decoder[A]]
    ): Decoder[A] =
      HList.fillWith[HD](Implicit).foldLeft(Decoder.failedWithMessage[A]("Empty decoder"))(DecoderOr)
  }

  val hListEncoder: Encoder[generic.HLists.Sample] = GenericEncodersOnceMore.genEncodeHList
  println("HlistEncoder " + io.circe.Printer.spaces2.print(hListEncoder(Sample(1, "Abc", Some(15)))))

  val coproductEncoder: Encoder[generic.Coproducts.Person] = GenericEncodersOnceMore.genEncodeCoproduct
  println("CoproductEncoder: " + io.circe.Printer.spaces2.print(coproductEncoder(Professor("proff", "Empl number"))))

  val coproductDecoder: Decoder[generic.Coproducts.Person] = GenericEncodersOnceMore.genCoproductDecoder
  println(
    "CoproductDecoder: " + coproductDecoder.decodeJson(
      Json.fromJsonObject(
        JsonObject(
          "name" -> "Abc".asJson,
          "indexNumber" -> "1234".asJson
        )
      )
    )
  )
}
