package rjs

import shapeless.{DepFn0, HList, HNil, ::}

trait HListOfEmptyLists[ In <: HList ] extends DepFn0 with Serializable

object HListOfEmptyLists {
  type Aux[ In <: HList, Out0 <: HList ] = HListOfEmptyLists[ In ] {type Out = Out0}

  def apply[ In <: HList ]( implicit ev: HListOfEmptyLists[ In ] ) = ev

  implicit def hnil: Aux[ HNil, HNil ] = new HListOfEmptyLists[ HNil ] {
    type Out = HNil

    def apply( ): Out = HNil
  }

  implicit def hcons[
  Head,
  Tail <: HList,
  TailOut <: HList
  ]( implicit
     tailOut: Aux[ Tail, TailOut ]
   ): Aux[ Head :: Tail, List[ Head ] :: TailOut ] = new HListOfEmptyLists[ Head :: Tail ] {
    type Out = ( List[ Head ] ) :: TailOut

    def apply( ): List[ Head ] :: TailOut = {
      List[ Head ]( ) :: tailOut( )
    }
  }
}
