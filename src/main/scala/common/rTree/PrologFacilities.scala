package common.rTree

import alice.tuprolog.{Prolog, Theory, Var}

/** Utility class of prolog helpers */
object PrologFacilities {

  /** Returns a prolog instance, after reading provided theories
    *
    * @param resource path where theories are written
    * @return a prolog engine
    */
  def getEngine(resource: String): Prolog = {
    val engine = new Prolog()
    val file = scala.io.Source.fromResource(resource)
    val lines = file.mkString
    file.close()
    engine.setTheory(new Theory(lines))
    engine
  }

  /** A prolog variable */
  object Variable {
    def apply(): Var = new Var("X")

    def apply(x: String): Var = new Var(x)
  }

  /** Simple factory for prolog double */
  object TuPrologDouble {
    type PrologDouble = alice.tuprolog.Double

    def apply(x: Double): PrologDouble = new PrologDouble(x)
  }

  /** Simple factory for an prolog integer */
  object TuPrologInt {
    type PrologInt = alice.tuprolog.Int

    def apply(x: Int): PrologInt = new PrologInt(x)
  }

}
