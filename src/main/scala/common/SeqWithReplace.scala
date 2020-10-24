package common

/**
 * Pimp my library pattern.
 */
object SeqWithReplace {

  implicit class ReplaceableSeq[A](seq: Seq[A]) {

    /** Return collection with an element replaced.
     *
     * @param elem    element to be replaced.
     * @param newElem element to insert.
     * @return new collection.
     */
    def replace(elem: A, newElem: A): Seq[A] = newElem +: seq.filterNot(_ == elem)

    /** Return collection with an element removed.
     *
     * @param elem element to be removed.
     * @return new collection.
     */
    def remove(elem: A): Seq[A] = seq.filterNot(_ == elem)
  }

}
