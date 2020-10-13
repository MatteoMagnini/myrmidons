package view.scene

import java.awt.{BorderLayout, Color}

import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.time.{TimeSeries, TimeSeriesCollection, Year}
import view._

import scala.swing.{Dimension, MainFrame}

private[view] case class TimeSeriesPanel(history: Map[Int, (Int, Int, Int, Int)])
  extends MainFrame {

  centerOnScreen()
  title = "Myrmidons - Report"

  val chart1: JFreeChart = ChartFactory.createTimeSeriesChart(
    "Myrmidons -  Insect plotter ", "LogicTime", "Value",
    setTimesSeries1(), true, true, true)
  val chart2: JFreeChart = ChartFactory.createTimeSeriesChart(
    "Myrmidons -  Anthill Food plotter ", "LogicTime", "Value",
    setTimesSeries2(), true, true, true)

  setChartColor(chart1)
  setChartColor(chart2)
  peer.getContentPane.add(new ChartPanel(chart1), BorderLayout.NORTH)
  peer.getContentPane.add(new ChartPanel(chart2), BorderLayout.SOUTH)
  size = new Dimension(SIMULATION_SIZE._1, SIMULATION_SIZE._2)
  visible = true

  private def setTimesSeries1(): TimeSeriesCollection = {
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

  private def setTimesSeries2(): TimeSeriesCollection = {
    val anthillFood = new TimeSeries("Anthill Food")
    val dataset: TimeSeriesCollection = new TimeSeriesCollection

    history.foreach { x =>
      anthillFood.add(new Year(x._1), x._2._4)
    }
    dataset.addSeries(anthillFood)
    dataset
  }

  private def setChartColor(chart1: JFreeChart): Unit = {
    val plot1: XYPlot = chart1.getXYPlot

    plot1.setBackgroundPaint(Color.WHITE)
    plot1.setDomainGridlinesVisible(true)
    plot1.setDomainGridlinePaint(Color.lightGray)
    plot1.setRangeGridlinePaint(Color.lightGray)

    val renderer = new XYLineAndShapeRenderer(true, true)
    plot1.setRenderer(renderer)
    renderer.setBaseShapesVisible(true)
    renderer.setBaseShapesFilled(true)
  }
}
