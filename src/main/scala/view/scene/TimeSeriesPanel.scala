package view.scene

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.Color

import org.jfree.data.time.{TimeSeries, TimeSeriesCollection, Year}
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import utility.Parameters.GUIConstant.SIMULATION_SIZE

import scala.swing.{Dimension, MainFrame}

private[view] case class TimeSeriesPanel(history: Map[Int, (Int, Int, Int)])
  extends MainFrame {

  centerOnScreen()
  title = "Myrmidons - Report"

  val chart: JFreeChart = ChartFactory.createTimeSeriesChart(
    "Myrmidons Entities plotter ", "LogicTime", "Value",
    setTimesSeries(), true, true, true)

  val plot: XYPlot = chart.getXYPlot

  plot.setBackgroundPaint(Color.WHITE)
  plot.setDomainGridlinesVisible(true)
  plot.setDomainGridlinePaint(Color.lightGray)
  plot.setRangeGridlinePaint(Color.lightGray)

  val renderer = new XYLineAndShapeRenderer(true, true)
  plot.setRenderer(renderer)
  renderer.setBaseShapesVisible(true)
  renderer.setBaseShapesFilled(true)

  peer.setContentPane(new ChartPanel(chart))
  size = new Dimension(SIMULATION_SIZE._1, SIMULATION_SIZE._2)
  visible = true

  private def setTimesSeries(): TimeSeriesCollection = {
    val foragingSeries = new TimeSeries("Foraging Ant")
    val patrollingSeries = new TimeSeries("Patrolling Ant ")
    val EnemiesSeries = new TimeSeries("Enemies ")
    val dataset: TimeSeriesCollection = new TimeSeriesCollection


    history.foreach { x =>
      foragingSeries.add(new Year(x._1), x._2._1)
      patrollingSeries.add(new Year(x._1), x._2._2)
      EnemiesSeries.add(new Year(x._1), x._2._3)
    }

    dataset.addSeries(foragingSeries)
    dataset.addSeries(patrollingSeries)
    dataset.addSeries(EnemiesSeries)

    dataset
  }
}
