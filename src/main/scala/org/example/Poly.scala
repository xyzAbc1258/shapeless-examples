package org.example

import cats.Show
import cats.implicits.toShow
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json, JsonObject}
import shapeless._

object Poly extends App {

  // MyCase1 is just a function from In to Out, tagged with type M
  trait MyCase1[M, In] {
    type Out

    def map(f: In): Out
  }

  //Just for Aux
  object MyCase1 {
    type Aux[M, I, O] = MyCase1[M, I] { type Out = O }
  }

  //MyPoly1 is a base trait for objects defining implicitly MyCase1, tagged with it's own type
  trait MyPoly1 {
    type Case[I, O] = MyCase1.Aux[this.type, I, O]
    def at[I, O](fun: I => O): Case[I, O] = new MyCase1[this.type, I] {
      override type Out = O
      override def map(f: I): O = fun(f)
    }
  }

  //Helper type tagged with M, which can transform HList HIn into HList HOut
  trait HListMapper[M, HIn <: HList] {
    type HOut <: HList
    def map(HIn: HIn): HOut
  }

  //HListMapper is constructed using appropriate MyCase1 tagged with same type as HListMapper
  object HListMapper {
    type Aux[M, I <: HList, O <: HList] = HListMapper[M, I] { type HOut = O }

    implicit def hnilMapper[M]: Aux[M, HNil, HNil] = new HListMapper[M, HNil] {
      override type HOut = HNil
      override def map(HIn: HNil): HNil = HIn
    }

    implicit def hconsMapper[M, H, T <: HList](implicit
        myCase: MyCase1[M, H],
        tail: HListMapper[M, T]
    ): Aux[M, H :: T, myCase.Out :: tail.HOut] = new HListMapper[M, H :: T] {
      override type HOut = myCase.Out :: tail.HOut

      override def map(HIn: H :: T): ::[myCase.Out, tail.HOut] = myCase.map(HIn.head) :: tail.map(HIn.tail)
    }

    //Just for syntax
    implicit class Ops[H <: HList](val h: H) extends AnyVal {
      def myMap(myPoly: Any)(implicit map: HListMapper[myPoly.type, H]): map.HOut = map.map(h)
    }
  }

  //Same as HListMapper, but for Coproducts
  trait CoproductMapper[M, HIn <: Coproduct] {
    type HOut <: Coproduct
    def map(HIn: HIn): HOut
  }

  object CoproductMapper {
    type Aux[M, I <: Coproduct, O <: Coproduct] = CoproductMapper[M, I] { type HOut = O }

    implicit def cnilMapper[M]: Aux[M, CNil, CNil] = new CoproductMapper[M, CNil] {
      override type HOut = CNil
      override def map(HIn: CNil): CNil = HIn
    }

    implicit def cconsMapper[M, H, T <: Coproduct](implicit
        myCase: MyCase1[M, H],
        tail: CoproductMapper[M, T]
    ): Aux[M, H :+: T, myCase.Out :+: tail.HOut] = new CoproductMapper[M, H :+: T] {
      override type HOut = myCase.Out :+: tail.HOut

      override def map(CIn: H :+: T): :+:[myCase.Out, tail.HOut] = CIn match {
        case Inl(head) => Inl(myCase.map(head))
        case Inr(t)    => Inr(tail.map(t))
      }

    }

    implicit class Ops[H <: Coproduct](val h: H) extends AnyVal {
      def myMap(myPoly: Any)(implicit map: CoproductMapper[myPoly.type, H]): map.HOut = map.map(h)
    }
  }

  //Function from Cp into some Out, tagged with M
  //A bit different than in shapeless, because here it also unifies coproduct
  trait MyFolder[M, Cp <: Coproduct] {
    type Out
    def fold(cp: Cp): Out
  }

  //Similarly we use MyCase1[M, _]
  object MyFolder {
    type Aux[M, C <: Coproduct, O] = MyFolder[M, C] { type Out = O }

    implicit def CNilFolder[M]: Aux[M, CNil, Nothing] = new MyFolder[M, CNil] {
      override type Out = Nothing

      override def fold(cp: CNil): Nothing = cp.impossible
    }

    implicit def cConsFolder[M, H, T <: Coproduct, O](implicit
        c: MyCase1.Aux[M, H, _ <: O],
        t: Aux[M, T, _ <: O]
    ): Aux[M, H :+: T, O] = new MyFolder[M, H :+: T] {
      override type Out = O

      override def fold(cp: H :+: T): O = cp.eliminate(c.map, t.fold)
    }

    implicit class MyFoldOps[C <: Coproduct](c: C) {
      def myFold(m: Any)(implicit folder: MyFolder[m.type, C]): folder.Out = folder.fold(c)
    }
  }

  trait MMapperLowPrio extends MyPoly1 {
    implicit def show[A: Show]: Case[A, String] = at(_.show)
  }

  object MMapper extends MMapperLowPrio {
    implicit val string: Case[String, String] = at(x => s""""$x" """)
  }

  implicit val showMMapper: Show[MMapper.type] = (_: MMapper.type) => "lovely 6object MMapper"

  import HListMapper.Ops

  val hlist = "abc" :: 12 :: MMapper :: HNil
  println("HList mapped by mapper " + hlist.myMap(MMapper))

  object Encode extends MyPoly1 {
    implicit def encode[A: Encoder]: Case[A, Json] = at(_.asJson)
  }

  import MyFolder.MyFoldOps

  type CP = String :+: Int :+: JsonObject :+: CNil
  val cp: CP = Inl("bla")
  val folder: MyFolder.Aux[Encode.type, CP, Json] = implicitly //called here explicitly to have correct out type
  val json: Json = cp.myFold(Encode)(folder)
  println("Cp encoded: " + io.circe.Printer.spaces2.print(Json.obj("value" -> json)))

}
