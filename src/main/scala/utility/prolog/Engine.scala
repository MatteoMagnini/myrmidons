package utility.prolog

import alice.tuprolog.{Prolog, Theory}

object Engine {

  /**
   * This method initializes the prolog engine.
   * Loads the whole theory from file and set of all rules.
   */
  def initializeProlog: Prolog = {
    val engine = new Prolog()
    val file = scala.io.Source.fromResource("R-tree.pl")
    val lines = file.mkString
    file.close()
    engine.setTheory(new Theory(lines))
    engine
  }
}
