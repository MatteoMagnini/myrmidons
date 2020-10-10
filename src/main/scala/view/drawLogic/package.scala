package view

package object drawLogic {

  implicit def doubleToFloat(value: Double): Float = value.toFloat

  implicit def doubleToInt(value: Double): Int = value.toInt

}
