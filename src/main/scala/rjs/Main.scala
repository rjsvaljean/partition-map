package rjs

import shapeless._

object Main {

  import rjs.Partition._

  def main( args: Array[ String ] ): Unit = {

    println( "----" )
    println( HListOfEmptyLists[ Int :: String :: HNil ].apply( ) )

    //  ----
    //  List() :: List() :: HNil

    println( "---" )
    sealed trait Animal extends Product with Serializable
    case class Cat( name: String ) extends Animal
    case class Dog( name: String ) extends Animal
    case class Rat( name: String ) extends Animal

    val animals = List( Cat( "Toby" ), Dog( "Ralph" ), Rat( "an" ), Dog( "Ron" ) )
    println(
      animals.partitionAuto
    )

    println(
      animals.partitionAuto(
        ( ( c: Cat ) => c.name ) ::
          ( ( d: Dog ) => d ) ::
          ( ( r: Rat ) => () ) ::
          HNil
      )
    )
    //  ----
    //  (List(Cat(Toby)),List(Dog(Ron), Dog(Ralph)),List(Rat(an)))
    //  (List(Toby),List(Dog(Ralph), Dog(Ron)),List(()))

    println( "---" )
    println(
      List[ Either[ Option[ String ], String ] ](
        Right( "a" ),
        Left( Some( "b" ) ),
        Left( None ),
        Right( "c" )
      ).partition(
        (List[ String ]( ), List[ String ]( ))
      )(
        _.fold(
          ( i: Option[ String ] ) => (i.toList, List( )),
          ( i: String ) => (List( ), List( i ))
        )
      )
    )

    //  ---
    //  (List(b),List(c, a))

  }
}
