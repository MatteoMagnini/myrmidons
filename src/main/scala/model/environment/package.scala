package model

import model.insects.info.InsectInfo

package object environment {

  implicit def extractOption[X](value: Option[X]): X = value.get

  implicit def entity2Id[X <: InsectInfo](entity: X): Int = entity.id
}
