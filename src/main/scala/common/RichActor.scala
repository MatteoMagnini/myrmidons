package common

import akka.actor.Actor.Receive
import akka.actor.ActorContext

object RichActor {

  /** Short signature to change behaviour.
   *
   * @param context to switch.
   */
  implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }

}
