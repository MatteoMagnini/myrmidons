package common.message

import model.environment.anthill.AnthillInfo
import model.environment.data.InsectReferences

object AnthillMessage {

  /**
   * Message sent from anthill to environment when all ants are created.
   *
   * @param ants number of ants
   */
  case class NewAnts(ants: InsectReferences) extends Message

  /** When anthill update is needed.
   * Message sent from anthill to environment when anthill information change.
   *
   * @param info anthill information
   */
  case class UpdateAnthill(info: AnthillInfo) extends Message


  /**
   * Message sent from anthill to ants when they reach it.
   *
   * @param antIsInsideTheAnthill true if the ant is inside, false otherwise
   */
  case class UpdateAnthillCondition(antIsInsideTheAnthill: Boolean) extends Message

}
