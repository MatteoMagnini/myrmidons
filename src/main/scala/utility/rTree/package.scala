package utility

import alice.tuprolog.Term
import utility.rTree.PrologFacilities.TuPrologDouble

package object rTree {

  implicit class RichTerm(t: Term){

    def getAsInt: Option[Int] =
      try {
        Some(t.toString.toInt)
      } catch {
        case _:Exception => None
      }
  }


}
