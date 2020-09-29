package view.scene

import model.Drawable
import model.Fights.Fight
import model.anthill.AnthillInfo
import model.environment.FoodPheromone
import model.environment.elements.{Food, Obstacle}
import model.insects.info.{EnemyInfo, ForagingAntInfo}
import view.scene.MemoHelper.memoize

import scala.swing.{Graphics2D, Panel}


/**
 * Panel that will be contain simulation entities
 * and its view behaviours.
 */

object MemoHelper {
  def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }
}

object singletonList {
  private val memoized: Any => Seq[Any] = memoize(x => {
    println(s" Calling singleton with input $x")
    Seq(x)
  })

  def apply[A](a: A): Seq[A] = {
    memoized(a).asInstanceOf[Seq[A]]
  }
}

case class MyrmidonsPanel() extends Panel {

  private var restartFlag = false

  import scala.ref.WeakReference


  private val flyweightData = new scala.collection.mutable.WeakHashMap[Drawable, WeakReference[Drawable]]()
  private var infoEntities: Seq[Object] = Seq.empty
  size.height = 800
  size.width = 800

  override def paintComponent(g: Graphics2D) {

    if (restartFlag) g.clearRect(0, 0, size.width, size.height)
    else {
      g.clearRect(0, 0, size.width, size.height)

      import view.drawLogic.DrawableEntities._

      infoEntities.foreach {

        case entity: ForagingAntInfo => draw(entity, g, size)

        case entity: Food => draw(entity, g, size)

        case entity: Obstacle => draw(entity, g, size)

        case entity: AnthillInfo => draw(entity, g, size)

        case entity: EnemyInfo => draw(entity, g, size)

        case entity: FoodPheromone => draw(entity, g, size)

        case entity: Fight[ForagingAntInfo, EnemyInfo] => draw(entity, g, size)

        case entity: List[Food] => println("BAH")
      }
    }
  }

  def draw_(): Unit = {
    repaint()
  }

  def clear(): Unit = {
    this.restartFlag = true
  }

  /**
   * Set all new position of entities in next frame.
   *
   * @param info Seq of all the entities that will be draw in panel.
   * @return number of ant.
   */
  def setEntities(info: Seq[Drawable]): (Int, Int) = {

    infoEntities = Seq.empty
    //infoEntities = info
    //TODO NON FUNZIONA NON DISEGNA SE NON FACCIO RIGA SOPRA
    info.foreach(x => infoEntities = singletonList(x) +:infoEntities)

    var antsEntities: Seq[ForagingAntInfo] = Seq.empty
    var anthillEntity: Option[AnthillInfo] = None

    infoEntities.foreach {
      case entity: ForagingAntInfo => antsEntities = entity +: antsEntities
      case entity: AnthillInfo => anthillEntity = Some(entity)
      case _ =>
    }
    (antsEntities.size,5)
     // anthillEntity.get.foodAmount.toInt)
  }

  /*def addWithCache(data: Drawable): Drawable = {
    import scala.ref.WeakReference
    if (!flyweightData.contains(data)) flyweightData.put(data, new WeakReference[Drawable](data))
    // return the single immutable copy with the given values
    flyweightData(data).get.get
  }*/
}
