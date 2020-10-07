package model.insects.competences

import akka.actor.Actor.Receive
import akka.actor.{ActorContext, ActorRef}
import model.insects.info.PatrollingAntInfo
import utility.Messages.Move
import utility.geometry.{OrientedVector2D, OrientedVector2DWithNoise}

/**
 * Specific competences suitable only for foraging ants
 */
trait PatrollingAntCompetences extends AntCompetences[PatrollingAntInfo]

case class DangerPheromoneTaxis() extends PatrollingAntCompetences {

  import utility.PheromoneSeq._
  import utility.Parameters.Competence._
  import utility.Parameters.PatrollingAnt._
  def apply(context: ActorContext, environment: ActorRef, insect: ActorRef, info: PatrollingAntInfo, behaviour: PatrollingAntInfo => Receive): Unit = {
    val delta = info.dangerPheromones.toStream.filter(p => p.position --> info.position < DANGER_PHEROMONE_RANGE).weightedSum(info.position)
    val data = info.updateEnergy(ENERGY_DANGER_PHEROMONE_TAXIS)
    val newDelta = OrientedVector2DWithNoise(delta./\, MAX_VELOCITY, NOISE) >> (data.inertia * 2)
    val newDelta2 = OrientedVector2D(newDelta./\, MAX_VELOCITY)
    environment.tell(Move(data.position, newDelta2), insect)
    context >>> behaviour(data.updateDangerPheromones(Seq.empty))
  }

  override def hasPriority(info: PatrollingAntInfo): Boolean =
    info.dangerPheromones.toStream.exists(p => p.position --> info.position < DANGER_PHEROMONE_RANGE)

}