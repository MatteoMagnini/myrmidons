package utility

import akka.actor.Actor.Receive
import akka.actor.ActorContext

object RichActor {

  implicit class RichContext(context: ActorContext) {
    def >>>(behaviour: Receive): Unit = context become behaviour
  }

}
