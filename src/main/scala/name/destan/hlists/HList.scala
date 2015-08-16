package name.destan.hlists

import scala.language.higherKinds

trait TypeLevelMagic {

sealed trait Bool {
  type If[IfTrue <: Up, IfFalse <: Up, Up] <: Up
}

type If[Condition <: Bool, IfTrue <: Up, IfFalse <: Up, Up] =
    Condition#If[IfTrue, IfFalse, Up]

sealed trait True extends Bool {
  type If[IfTrue <: Up, IfFalse <: Up, Up] = IfTrue
}

sealed trait False extends Bool {
  type If[IfTrue <: Up, IfFalse <: Up, Up] = IfFalse
}

sealed trait Natural {
  type Match[IfZero <: Up, IfNonZero[N <: Natural] <: Up, Up]
}

sealed trait _0 extends Natural {
  type Match[IfZero <: Up, IfNonZero[N <: Natural] <: Up, Up] = IfZero
}

sealed trait Successor[N <: Natural] extends Natural {
    type Match[IfZero <: Up, IfNonZero[N <: Natural] <: Up, Up] = IfNonZero[N]
}

type _1 = Successor[_0]
type _2 = Successor[_1]
type _3 = Successor[_2]
type _4 = Successor[_3]
type _5 = Successor[_4]
type _6 = Successor[_5]
type _7 = Successor[_6]
type _8 = Successor[_7]
type _9 = Successor[_8]

sealed trait HList {
  type Head
  type Tail <: HList
  type Append[L <: HList] <: HList

  def head: Head
  def tail: Tail
  def ++[L <: HList](list: L): Append[L]
  def ::[V](value: V) = HCons(value, this)
}

case object HNil extends HList {
  type Head = Nothing
  type Tail = Nothing
  type Append[L <: HList] = L

  def head: Head = throw new NoSuchElementException()
  def tail: Tail = throw new NoSuchElementException()
  def ++[L <: HList](list : L): Append[L] = list
}

case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  type Head = H
  type Tail = T
  type Append[L <: HList] = HCons[Head, Tail#Append[L]]

  def ++[L <: HList](list : L): Append[L] = HCons(head, (tail ++ list))
}

}
