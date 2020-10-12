package utility.r_tree

import java.io.InputStream

import alice.tuprolog.{Prolog, Theory, Var}


object PrologFacilities {

  def getEngine(theories: InputStream*):Prolog = {
    val engine = new Prolog()
    theories foreach { f => engine addTheory new Theory(f) }
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
