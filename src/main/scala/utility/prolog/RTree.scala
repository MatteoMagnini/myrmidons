package utility.prolog

import alice.tuprolog.{Prolog, Struct, Term, Var}
import model.environment.pheromones.Pheromone
import model.environment.pheromones.INFLUENCE_RADIUS
import utility.geometry.Vector2D

class RTree(val engine: Prolog, val tree: Term) {

  def insert(id: Int, pheromone: Pheromone): RTree = {
    val query = new Struct("insert",(id,pheromone),tree,new Var("R"))
    val newTree = engine.solve(query).getTerm("R")
    RTree(engine, newTree)
  }

  def remove(id: Int, pheromone: Pheromone): RTree = {
    val query = new Struct("removeWithFix",(id,pheromone),tree,new Var("R"))
    val newTree = engine.solve(query).getTerm("R")
    RTree(engine, newTree)
  }

  def neighbours(position: Vector2D): String = {
    val query = new Struct("queryToList",tree, position.x, position.y, new Var("R"))
    val ids = engine.solve(query).getTerm("R").toString
    ids.split()
  }

  private implicit def pheromoneToNode(pair:(Int,Pheromone)): Term = {
    val x: String = pair._2.position.x
    val y: String = pair._2.position.y
    s"node(${pair._1},${x},${y})"
  }

  private implicit def DoubleToStringRange(value: Double): String = {
    s"range(${value - INFLUENCE_RADIUS},${value + INFLUENCE_RADIUS})"
  }

  private implicit def DoubleToRange(value: Double): Term = {
    s"range(${value - INFLUENCE_RADIUS},${value + INFLUENCE_RADIUS})"
  }

  private implicit def stringToTerm(value: String): Term = {
    engine.toTerm(value)
  }
}

object RTree {
  def apply(engine: Prolog): RTree = new RTree(engine, engine.toTerm("nil"))
  def apply(engine: Prolog, tree: Term): RTree = new RTree(engine, tree)
}
