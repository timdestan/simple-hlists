import org.scalatest._
import name.destan.hlists._

class HListSpec extends FlatSpec with Matchers {
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
