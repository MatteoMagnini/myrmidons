package model.insects

import model.insects.info.AntInfo

trait Ant[A <: AntInfo[A]] extends Insect[A]
