package utility

import alice.tuprolog.{Struct, Term}
import model.environment.pheromones._
import utility.geometry.Vector2D
import utility.rTree.PrologFacilities.{TuPrologDouble, TuPrologInt}
import utility.rTree.RTree.{MyRange, Node, NotEmptyTree, Tree}

package object rTree {

  val range = "range"
  val node = "node"
  val tree = "tree"
  val nil = "nil"
  val none = "none"

  implicit class RichTerm(term: Term){

    def getAsInt: Option[Int] =
      try {
        Some(term.toString.toInt)
      } catch {
        case _:Exception => None
      }

    def getAsNode: Node = {
      val struct = term.getTerm.asInstanceOf[Struct]
      Node(struct.getArg(0).getAsInt, struct.getArg(1) getAsRange, struct.getArg(2) getAsRange)
    }

    def getAsRange: (Double, Double) = {
      val struct = term.getTerm.asInstanceOf[Struct]
      (struct.getArg(0).toString.toDouble, struct.getArg(1).toString.toDouble)
    }

    def getAsTree: Tree = term match {
      case x if x.isCompound =>
        val struct = x.getTerm.asInstanceOf[Struct]
        println(struct)
        val root = struct.getArg(1).getAsNode
        Tree(struct.getArg(0) getAsTree, root, struct.getArg(2) getAsTree)
      case _ => Tree()
    }

    def getAsList: List[Term] = term match {
      case x if x.isCompound =>
        val struct = x.getTerm.asInstanceOf[Struct]
        struct.listHead() :: struct.listTail().getAsList
      case _ => Nil
    }
  }

  implicit def getNodeAsTerm(n: Node): Term = n.id match {
    case Some(_) => new Struct(node, TuPrologInt(n.id.get), n.rangeX, n.rangeY)
    case _ => new Struct(node, Term.createTerm(none), n.rangeX, n.rangeY)
  }

  implicit def getRangeAsTerm(r: MyRange): Term =
    new Struct(range, TuPrologDouble(r._1), TuPrologDouble(r._2))

  implicit def getTreeAsTerm(t: Tree): Term = t match {
    case x: NotEmptyTree => new Struct(tree, getTreeAsTerm(t.left), t.root.get, getTreeAsTerm(t.right))
    case _ => Term.createTerm(nil)
  }

  implicit def getPheromoneAsNode(p: (Int,Pheromone)): Node = {
    val ranges = p._2.position.rangeOfInfluence(INFLUENCE_RADIUS)
    Node(p._1, ranges._1, ranges._2)
  }

  implicit def toOption[X](value:X): Option[X] = Some(value)

  implicit class RichPosition(p: Vector2D) {
    def rangeOfInfluence(influence: Double): (MyRange, MyRange) = {
      ((p.x - influence, p.x + influence), (p.y - influence, p.y + influence))
    }
  }
}
