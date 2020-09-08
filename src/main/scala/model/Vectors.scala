package model

case class RichTuple(x:Double, y:Double) {
  import TupleOp._

  def + (delta: RichTuple): (Double, Double) = (x + delta.x, y + delta.y)

  def - : (Double, Double) = (-x, -y)

  def - (delta: RichTuple): (Double, Double) = (x - delta.x, y - delta.y)

  def || : Double = math.sqrt(x * x + y * y)

  def |-|(other: RichTuple) : Double = this - other ||

  def >> (direction: (Int, Int)): (Double, Double) = (x + direction.x, y + direction.y)

}

object TupleOp {
  implicit def toRichTuple(value: (Double, Double)): RichTuple = RichTuple(value._1, value._2)
  implicit def inTtoRichTuple(value: (Int, Int)): RichTuple = RichTuple(value._1.toDouble, value._2.toDouble)
}

