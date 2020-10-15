package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import model.insects.Ants.PatrollingAnt._
import model.insects.info.PatrollingAntInfo
import utility.Messages.Move
import utility.geometry.{OrientedVector2D, OrientedVector2DWithNoise}
import utility.RichActor._

/**
 * Specific competences suitable only for foraging ants.
 */
trait PatrollingAntCompetences extends AntCompetences[PatrollingAntInfo]

/**
 * Competence that enable a foraging ant to follow the traces of (danger) pheromones.
 *
 * @param behaviour of the ant
 */
case class DangerPheromoneTaxis(behaviour: PatrollingAntInfo => Receive) extends PatrollingAntCompetences {

  import utility.PheromoneSeq._

  override def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: PatrollingAntInfo): Unit = {
    val delta = info.dangerPheromones.toStream
      .filter(p => p.position --> info.position < DANGER_PHEROMONE_RANGE)
      .weightedSum(info.position)
    val data = info.updateEnergy(ENERGY_DANGER_PHEROMONE_TAXIS)
    val newDelta = OrientedVector2DWithNoise(delta./\, MAX_VELOCITY, NOISE) >> (data.inertia * INERTIA_FACTOR_IN_TAXIS)
    val newDelta2 = OrientedVector2D(newDelta./\, MAX_VELOCITY)
    environment.tell(Move(data.position, newDelta2), insect)
    context >>> behaviour(data.updateDangerPheromones(Seq.empty))
  }

  override def hasPriority(info: PatrollingAntInfo): Boolean =
    info.dangerPheromones.toStream.exists(p => p.position --> info.position < DANGER_PHEROMONE_RANGE)

}