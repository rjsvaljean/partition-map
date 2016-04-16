# partition-map

Say you have a 

```
val in = List(Right("a"), Left(1))
```

I feel that this:

```
val (rights, lefts) = in.partition(_.isRight)
(rights.collect { case Right(i) => i }, lefts.collect { case Left(i) => i })
// res16: (List[String], List[Int]) = (List(a),List(1))
```

is unnecessarily cumbersome (and inefficient)

and this:

```
in.foldLeft((List[String](), List[Int]())) {
  case ((rights, lefts), Right(x)) => (x :: rights, lefts)
  case ((rights, lefts), Left(x)) => (rights, x :: lefts)
}
// res18: (List[String], List[Int]) = (List(a),List(1))
```

is a bit too permissive

So, I wrote a thing to allow this:

```
in.partition((List[String](), List[Int]())) {
  case Right(x) => (List(x), List())
  case Left(y) => (List(), List(y)) 
}
// res20: (List[String], List[Int]) = (List(a),List(1))
```

# Other things

### Auto Partitioning based on derived `shapeless.Coproduct`

```
sealed trait Animal extends Product with Serializable
case class Cat( name: String ) extends Animal
case class Dog( name: String ) extends Animal
case class Rat( name: String ) extends Animal

val animals = List( Cat( "Toby" ), Dog( "Ralph" ), Rat( "an" ), Dog( "Ron" ) )
```

```
import rjs.Partition._
assert(
  animals.partitionAuto ==
  (
    List(Cat("Toby")),
    List(Dog("Ralph"), Dog("Ron")),
    List(Rat("an"))
  )
)
```

A bit more control:

```
assert(
  animals.partitionAuto(
    ( ( c: Cat ) => c.name ) ::
      ( ( d: Dog ) => d ) ::
      ( ( r: Rat ) => () ) ::
      HNil
  ) ==
  (
    List("Toby"),
    List(Dog("Ralph"), Dog("Ron")),
    List(())
  )
)
```

### More aribtrary partitioning

```
assert(
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
) ==

(
  List("b"),
  List("a", "c")
)
)
 ```
