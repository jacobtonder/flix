package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.ast.TypedAst.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Type._

import scala.collection.immutable

object FMap {

  /**
    * All map operations.
    */
  val Ops: immutable.Map[Name.Resolved, MapOperator] = List(
    "Map::null" -> nul,
  // TODO: Empty, singleton
    "Map::memberOf" -> memberOf,
    "Map::lookup" -> lookup,
  // TODO: LookupWithDefault
    "Map::insert" -> insert,
  // TODOL insertWith
  // TODO: insertWithKey
    // TODO: adjust
    // TODO: adjustWithKey

    "Map::update" -> update,
// TODO: Update with key
    "Map::delete" -> delete,
// TODO: alter
    "Map::union" -> union,
    "Map::intersection" -> intersection,
    "Map::difference" -> difference,
    "Map::map" -> map,
    "Map::mapWithKey" -> mapWithKey,
    "Map::toAscList" -> toAscList,
    "Map::toDescList" -> toDescList,
    "Map::toSet" -> toSet
  ).map {
    case (name, op) => Name.Resolved.mk(name) -> op
  }.toMap


  // TODO - removeKey
  //TODO  - foldValues
  // TODO filterKeys/filterValues

  // TODO - mapKeys
  // TODO  foldLeft/foldRigyht, foldLeftWithKey, foldRightWithKey
  // TODO  - elms
  // TODO  - keys
  // TODO  filter/filterWithKey
  // TODO  paritition, partitionWithKey
  // TODO  isSubmapOf
  // TODO  isProperSubmapOf


  /**
    * A common super-type for all map operations.
    */
  sealed trait MapOperator extends LibraryOperator

  /**
    * Generic type variables.
    */
  val K = Type.Var("K")
  val V = Type.Var("V")
  val A = Type.Var("A")
  val B = Type.Var("B")

  /////////////////////////////////////////////////////////////////////////////
  // Basic Operations                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `null : Map[K, V] => Bool` function.
    */
  object nul extends MapOperator {
    val tpe = Map(K, V) ~> Bool
  }

  /**
    * The `memberOf : (K, Map[K, V]) => Bool` function.
    */
  object memberOf extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Bool
  }

  /**
    * The `lookup : (K, Map[K, V]) => Opt[V]` function.
    */
  object lookup extends MapOperator {
    val tpe = (K, Map(K, V)) ~> Opt(V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Insert / Update / Delete                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `insert : (K, V, Map[K, V]) => Map[K, V]` function.
    */
  object insert extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `update : (K, V => V, Map[K, V]) => Map[K, V]` function.
    */
  object update extends MapOperator {
    val tpe = (K, V ~> V, Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `delete : (K, V, Map[K, V]) => Map[K, V]` function.
    */
  object delete extends MapOperator {
    val tpe = (K, V, Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Combine                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `union : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object union extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `intersection : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object intersection extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /**
    * The `difference : (Map[K, V], Map[K, V]) => Map[K, V]` function.
    */
  object difference extends MapOperator {
    val tpe = (Map(K, V), Map(K, V)) ~> Map(K, V)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Map                                                                     //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `map : (A => B, Map[K, A]) => Map[K, B]` function.
    */
  object map extends MapOperator {
    val tpe = (A ~> B, Map(K, A)) ~> Map(K, B)
  }

  /**
    * The `mapWithKey : ((K, A) => B, Map[K, A]) => Map[K, B]` function.
    */
  object mapWithKey extends MapOperator {
    val tpe = ((K, A) ~> B, Map(K, A)) ~> Map(K, B)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Conversions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The `toAscList : Map[K, V] => Lst[(K, V)]` function.
    */
  object toAscList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  /**
    * The `toDescList : Map[K, V] => Lst[(K, V)]` function.
    */
  object toDescList extends MapOperator {
    val tpe = Map(K, V) ~> Lst((K, V))
  }

  /**
    * The `toSet : Map[K, V] => Set[(K, V)]` function.
    */
  object toSet extends MapOperator {
    val tpe = Map(K, V) ~> Set((K, V))
  }


}
