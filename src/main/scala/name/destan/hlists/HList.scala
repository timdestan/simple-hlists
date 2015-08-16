package name.destan.hlists

import scala.language.higherKinds

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
 