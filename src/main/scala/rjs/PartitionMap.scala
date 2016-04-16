package rjs

import shapeless._

object Partition {

  object combineLists extends Poly1 {
    implicit def caseX[ T ] = at[ (List[ T ], List[ T ]) ]( i => i._2 ++ i._1 )
  }

  object optionToList extends Poly1 {
    implicit def caseX[ T ] = at[ Option[ T ] ]( _.toList )
  }

  object unboxList extends Poly1 {
    implicit def caseX[ T ] = at[ List[ T ] ]( _.head /* Never evaluated*/ )
  }

  object applyFns extends Poly1 {
    implicit def caseX[ T, S ] = at[ (Option[ T ], T => S) ] {
      case (t, f) => t.map( f )
    }
  }

  private val applyFnsToList = applyFns andThen optionToList

  implicit class ListOps[ T ]( ts: List[ T ] ) {
    def partition[
      Out,
      OutAsHList <: HList,
      ZipperOut <: HList,
      MapperOut <: HList
    ](
      acc: Out
    )(
      foldFn: T => Out
    )( implicit
      outGen: Generic.Aux[ Out, OutAsHList ],
      zipper: ops.hlist.Zip.Aux[ OutAsHList :: OutAsHList :: HNil, ZipperOut ],
      f: ops.hlist.Mapper.Aux[ combineLists.type, ZipperOut, OutAsHList ]
    ) = ts.foldLeft( acc ) { ( acc: Out, t: T ) =>
      val newData: Out = foldFn( t )
      val newDataAsHList: OutAsHList = outGen.to( newData )
      val accAsHList: OutAsHList = outGen.to( acc )
      val newAccAsHList: OutAsHList = newDataAsHList.zip( accAsHList ).map( combineLists )
      val newAcc: Out = outGen.from( newAccAsHList )
      newAcc
    }


    def partitionAuto[
      TAsCoproduct <: Coproduct,
      TAsHList <: HList,
      TAsHListOfOptions <: HList,
      Out,
      OutAsHList <: HList,
      ZipperOut <: HList,
      MapperOut <: HList
    ]( implicit
      gen: Generic.Aux[ T, TAsCoproduct ],
      toOptions: CoproductAsHList.Aux[ TAsCoproduct, TAsHListOfOptions ],
      toOut: ops.hlist.Mapper.Aux[ optionToList.type, TAsHListOfOptions, OutAsHList ],
      asHList: ops.coproduct.ToHList.Aux[ TAsCoproduct, TAsHList ],
      outAsHList: HListOfEmptyLists.Aux[ TAsHList, OutAsHList ],
      retuple: ops.hlist.Tupler.Aux[ OutAsHList, Out ],
      outGen: Generic.Aux[ Out, OutAsHList ],
      zipper: ops.hlist.Zip.Aux[ OutAsHList :: OutAsHList :: HNil, ZipperOut ],
      f: ops.hlist.Mapper.Aux[ combineLists.type, ZipperOut, OutAsHList ]
     ): Out = ts.foldLeft( outGen.from( outAsHList.apply( ) ) ) { ( acc: Out, t: T ) =>
      val newDataAsHList: OutAsHList = CoproductAsHList[ TAsCoproduct ].apply( gen.to( t ) ).map( optionToList )
      val accAsHList: OutAsHList = outGen.to( acc )
      val newAccAsHList: OutAsHList = newDataAsHList.zip( accAsHList ).map( combineLists )
      val newAcc: Out = outGen.from( newAccAsHList )
      newAcc
    }

    def partitionAuto[
      TAsCoproduct <: Coproduct,
      TAsHList <: HList,
      TAsHListOfOptions <: HList,
      Out,
      OutAsHList <: HList,
      PreAccType <: HList,
      ZipperOut <: HList,
      Fns <: HList
    ](
      fns: Fns
    )( implicit
      gen: Generic.Aux[ T, TAsCoproduct ],
      toOptions: CoproductAsHList.Aux[ TAsCoproduct, TAsHListOfOptions ],
      zipTWithFns: ops.hlist.Zip.Aux[ TAsHListOfOptions :: Fns :: HNil, PreAccType ],
      applyFns: ops.hlist.Mapper.Aux[ applyFnsToList.type, PreAccType, OutAsHList ],
      outTyper: ops.hlist.Mapper.Aux[ unboxList.type, OutAsHList, TAsHList ],
      outAsHList: HListOfEmptyLists.Aux[ TAsHList, OutAsHList ],
      retuple: ops.hlist.Tupler.Aux[ OutAsHList, Out ],
      outGen: Generic.Aux[ Out, OutAsHList ],
      zipper: ops.hlist.Zip.Aux[ OutAsHList :: OutAsHList :: HNil, ZipperOut ],
      combineZippedLists: ops.hlist.Mapper.Aux[ combineLists.type, ZipperOut, OutAsHList ]
    ): Out = ts.foldLeft( outGen.from( outAsHList.apply( ) ) ) { ( acc: Out, t: T ) =>
      val newDataAsHList: OutAsHList = CoproductAsHList[ TAsCoproduct ].apply( gen.to( t ) ).zip( fns ).map( applyFnsToList )
      val accAsHList: OutAsHList = outGen.to( acc )
      val newAccAsHList: OutAsHList = newDataAsHList.zip( accAsHList ).map( combineLists )
      val newAcc: Out = outGen.from( newAccAsHList )
      newAcc
    }

  }

}





