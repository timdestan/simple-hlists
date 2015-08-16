import org.scalatest._
import name.destan.hlists._

class HListSpec extends FlatSpec with Matchers with TypeLevelMagic {
  def areEqual[A,B](implicit witness:(A =:= B) = null) = witness != null

  "Booleans" should "allow If testing" in {
    areEqual[If[True, _2, _1, Natural], _2] should be (true)
    areEqual[If[False, _1, _2, Natural], _2] should be (true)
    areEqual[If[True, _1, _2, Natural], _2] should be (false)
    areEqual[If[False, _2, _1, Natural], _2] should be (false)
  }

  "HNil" should "raise exception if head or tail called" in {
    a [NoSuchElementException] should be thrownBy {
      HNil.head
    }
    a [NoSuchElementException] should be thrownBy {
      HNil.tail
    }
  }

  "HCons" should "allow retrieval of head and tail" in {
    ("foo" :: 7 :: HNil).tail.head should be (7)
  }

  "append" should "append to HLists together" in {
    val appended = ("foo" :: false :: HNil) ++ (7 :: 3.14 :: HNil)
    appended.tail.tail.head should be (7)
  }
}
