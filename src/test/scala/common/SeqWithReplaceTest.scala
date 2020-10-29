package common

import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import common.SeqWithReplace._

class SeqWithReplaceTest extends AnyWordSpecLike with BeforeAndAfter {

  "a sequence" when {

    val element1: Int = 1
    val element2: Int = 2
    val element3: Int = 3
    val sequence: Seq[Int] = Seq(element1,element2,element3)

    "an element is removed" should {
      val removedSequence = sequence.remove(element1)

      "not contain removed element" in {
        assert(!removedSequence.contains(element1))
      }

      "an element is replaced" should {
        val newElement: Int = 4
        val replacedSequence = sequence.replace(element2, newElement)

        "contain new element but not replaced" in {
          assert(replacedSequence.contains(newElement))
          assert(!replacedSequence.contains(element2))
        }
      }

    }
  }
}
