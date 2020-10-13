package view.frame

import java.awt.{BorderLayout, Color}

import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.time.{TimeSeries, TimeSeriesCollection, Year}
import view.SIMULATION_SIZE

import scala.swing.{Dimension, MainFrame}

/**
 * Main frame that contains time series report of simulation.
 *
 * @param history simulation history until the Repaint button click.
 */
private[view] case class TimeSeriesFrame(history: Map[Int, (Int, Int, Int, Int)])
  extends MainFrame {

  centerOnScreen()
  title = "Myrmidons - Report"

  val insectChart: JFreeChart = ChartFactory.createTimeSeriesChart(
    "Myrmidons -  Insect number plotter ", "LogicTime", "Value",
    setInsectSeries(), true, true, true)
  val foodChart: JFreeChart = ChartFactory.createTimeSeriesChart(
    "Myrmidons -  Anthill food plotter ", "LogicTime", "Value",
    setAnthillFoodSeries(), true, true, true)

  setChartColor(insectChart)
  setChartColor(foodChart)
  peer.getContentPane.add(new ChartPanel(insectChart), BorderLayout.NORTH)
  peer.getContentPane.add(new ChartPanel(foodChart), BorderLayout.SOUTH)
  size = new Dimension(SIMULATION_SIZE._1, SIMULATION_SIZE._2)
  visible = true

  /**
   * Create time series collection with insect number history from simulation data.
   *
   * @return insect number series.
   */
  private def setInsectSeries(): TimeSeriesCollection = {
    val foragingSeries = new TimeSeries("Foraging Ant")
    val patrollingSeries = new TimeSeries("Patrolling Ant ")
    val enemiesSeries = new TimeSeries("Enemies ")
    val dataset: TimeSeriesCollection = new TimeSeriesCollection
    history.foreach { x =>
      foragingSeries.add(new Year(x._1), x._2._1)
      patrollingSeries.add(new Year(x._1), x._2._2)
      enemiesSeries.add(new Year(x._1), x._2._3)
    }
    dataset.addSeries(foragingSeries)
    dataset.addSeries(patrollingSeries)
    dataset.addSeries(enemiesSeries)
    dataset
  }

  /**
   * Create time series collection with anthill food history from simulation data.
   *
   * @return anthill food series.
   */
  private def setAnthillFoodSeries(): TimeSeriesCollection = {
    val anthillFood = new TimeSeries("Anthill Food")
    val dataset: TimeSeriesCollection = new TimeSeriesCollection
    history.foreach { x => anthillFood.add(new Year(x._1), x._2._4) }
    dataset.addSeries(anthillFood)
    dataset
  }

  /**
   * Set chart properties.
   *
   * @param chart to set background and lines.
   */
  private def setChartColor(chart: JFreeChart): Unit = {
    val plot: XYPlot = chart.getXYPlot
    val renderer = new XYLineAndShapeRenderer(true, true)
    plot.setBackgroundPaint(Color.WHITE)
    plot.setDomainGridlinesVisible(true)
    plot.setDomainGridlinePaint(Color.lightGray)
    plot.setRangeGridlinePaint(Color.lightGray)
    plot.setRenderer(renderer)
    renderer.setBaseShapesVisible(true)
    renderer.setBaseShapesFilled(true)
  }
}
