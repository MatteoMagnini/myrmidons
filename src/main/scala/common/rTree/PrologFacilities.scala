package common.rTree

import alice.tuprolog.{Prolog, Theory, Var}

object PrologFacilities {

  def getEngine(resource: String):Prolog = {
    val engine = new Prolog()
    val file = scala.io.Source.fromResource(resource)
    val lines = file.mkString
    file.close()
    engine.setTheory(new Theory(lines))
    engine
  }

  object Variable {
    def apply(): Var = new Var("X")
    def apply(x:String): Var = new Var(x)
  }

  object TuPrologDouble {
    type PrologDouble = alice.tuprolog.Double
    def apply(x: Double): PrologDouble = new PrologDouble(x)
  }

  object TuPrologInt {
    type PrologInt = alice.tuprolog.Int
    def apply(x: Int): PrologInt = new PrologInt(x)
  }
}
