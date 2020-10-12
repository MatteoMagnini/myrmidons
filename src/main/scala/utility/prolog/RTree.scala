package utility.prolog

import alice.tuprolog.{Prolog, Struct, Term, Var}
import model.environment.pheromones.Pheromone
import model.environment.pheromones.INFLUENCE_RADIUS

class RTree(val engine: Prolog, val tree: Term) {

  def insert(id: Int, pheromone: Pheromone): RTree = {
    val query = new Struct("insert",(id,pheromone),tree,new Var("R"))
    val newTree = engine.solve(query).getTerm("R")
    RTree(engine, newTree)
  }

  def remove(id: Int, pheromone: Pheromone): RTree = {
    val query = new Struct("remove",(id,pheromone),tree,new Var("R"))
    val newTree = engine.solve(query).getTerm("R")
    RTree(engine, newTree)
  }

  private implicit def pheromoneToNode(pair:(Int,Pheromone)): Term = {
    val x = pair._2.position.x
    val y = pair._2.position.y
    val rangeX = s"range(${x - INFLUENCE_RADIUS},${x + INFLUENCE_RADIUS})"
    val rangeY = s"range(${y - INFLUENCE_RADIUS},${y + INFLUENCE_RADIUS})"
    s"node(${pair._1},${rangeX},${rangeY})"
  }

  private implicit def stringToTerm(value: String): Term = {
    engine.toTerm(value)
  }

}

object RTree {
  def apply(engine: Prolog): RTree = new RTree(engine, engine.toTerm("nil"))
  def apply(engine: Prolog, tree: Term): RTree = new RTree(engine, tree)
}
