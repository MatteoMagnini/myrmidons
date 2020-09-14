package utility

object SeqWithReplace {

  implicit class ReplaceableSeq[A](seq: Seq[A]) {

    def replace(elem:A, newElem:A): Seq[A] = newElem +: seq.filterNot(_ == elem)
  }
}
