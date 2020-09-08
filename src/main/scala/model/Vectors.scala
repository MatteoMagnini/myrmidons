package model

case class RichTuple(value: (Double, Double)) {
  import TupleOp._

  def + (delta: RichTuple): (Double, Double) = (value._1 + delta.value._1, value._2 + delta.value._2)

  def - : (Double, Double) = (- value._1, - value._2)

  def - (delta: RichTuple): (Double, Double) = (value._1 - delta.value._1, value._2 - delta.value._2)

  def || : Double = math.sqrt(value._1 * value._1 + value._2 * value._2)

  def |-|(other:RichTuple) : Double = value - other ||

  def >> (direction: (Int, Int)): (Double, Double) = (value._1 + direction._1, value._2 + direction._2)

}

object TupleOp {
  implicit def toRichTuple(value: (Double, Double)): RichTuple = RichTuple(value)
  implicit def inTtoRichTuple(value: (Int, Int)): RichTuple = RichTuple(value._1.toDouble, value._2.toDouble)
}

