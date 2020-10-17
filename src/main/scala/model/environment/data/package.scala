package model.environment

import akka.actor.ActorRef

package object data {

  type InsectReferences = Map[Int, ActorRef]
}
