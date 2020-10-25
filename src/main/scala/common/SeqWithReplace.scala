package common

/**
 * Pimping sequence with remove and replace functions.
 */
object SeqWithReplace {

  implicit class ReplaceableSeq[A](seq: Seq[A]) {

    /** Return sequence with an element replaced.
     *
     * @param elem    element to be replaced.
     * @param newElem element to insert.
     * @return new sequence.
     */
    def replace(elem: A, newElem: A): Seq[A] = newElem +: seq.filterNot(_ == elem)

    /** Return sequence with an element removed.
     *
     * @param elem element to be removed.
     * @return new sequence.
     */
    def remove(elem: A): Seq[A] = seq.filterNot(_ == elem)
  }

}
