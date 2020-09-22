package model.insects

import akka.actor.{Actor, ActorLogging, ActorRef}

/**
  * An insect is an entity with its own behaviour.
  * For this reason it extends Actor, it has its own control flow and is reactive to inputs (messages).
  * It also holds the information (state) of the insect.
  */

trait Insect extends Actor with ActorLogging {

  def info: InsectInfo
  def environment: ActorRef
}
