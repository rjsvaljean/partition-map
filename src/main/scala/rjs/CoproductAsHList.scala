package rjs

import shapeless._

trait CoproductAsHList[ L <: Coproduct ] extends DepFn1[ L ] with Serializable {
  type Out <: HList
}

object CoproductAsHList {
  def apply[ L <: Coproduct ]( implicit thl: CoproductAsHList[ L ] ): Aux[ L, thl.Out ] = thl

  type Aux[ L <: Coproduct, Out0 <: HList ] = CoproductAsHList[ L ] {type Out = Out0}

  implicit val cnilAsHList: Aux[ CNil, HNil ] =
    new CoproductAsHList[ CNil ] {
      type Out = HNil

      def apply( t: CNil ): Out = HNil
    }

  implicit def cconsAsHList[ H, T <: Coproduct ]( implicit ut: CoproductAsHList[ T ] ): Aux[ H :+: T, Option[ H ] :: ut.Out ] =
    new CoproductAsHList[ H :+: T ] {
      type Out = Option[ H ] :: ut.Out

      def apply( t: :+:[ H, T ] ): Out = {
        if ( null == t ) ( None: Option[ H ] ) :: ut.apply( null.asInstanceOf[ T ] )
        else t match {
          case Inl( head ) => Option( head ) :: ut.apply( null.asInstanceOf[ T ] )
          case Inr( tail ) => ( None: Option[ H ] ) :: ut.apply( tail )
        }
      }
    }
}
