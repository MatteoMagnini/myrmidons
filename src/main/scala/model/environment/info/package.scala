package model.environment

import akka.actor.ActorRef

package object info {

  type InsectReferences = Map[Int, ActorRef]
}
