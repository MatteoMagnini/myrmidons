package view

package object actor {

  val REPORT_NAME: String = "SimulationReport.json"
  val DEFAULT_RATE : Int = 50
  val REPORT_INC_CLOCK : Int = 20

  implicit def intToString(value: Int): String = value.toString

}
