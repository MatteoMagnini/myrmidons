package view

package object actor {

  val REPORT_NAME: String = "SimulationReport.json"
  val DEFAULT_RATE : Int = 30

  implicit def intToString(value: Int): String = value.toString

}
