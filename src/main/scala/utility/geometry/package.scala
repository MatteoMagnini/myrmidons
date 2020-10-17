package utility

package object geometry {

  /** Implicit conversions for [[utility.geometry.Vector2D]] instances
    *
    * {{{
    * val v = (1, 2) >> (3, 4)
    * }}}
    */
    implicit def toVec2D(value: (Double, Double)): Vector2D = Vector2D(value._1, value._2)

    implicit def intToVec2D(value: (Int, Int)): Vector2D = Vector2D(value._1.toDouble, value._2.toDouble)

  /** Implicit conversion to convert [[utility.geometry.Vector3D]] instances
    *
    * {{{
    * val v = (1, 2, 1) >> (3, 4, 1)
    * }}}
    * */
    implicit def toVec3D(value: (Double, Double, Double)): Vector3D = Vector3D(value._1, value._2, value._3)

    implicit def intToVec3D(value: (Int, Int, Int)): Vector3D =
      Vector3D(value._1.toDouble, value._2.toDouble, value._3.toDouble)


  /** Implicit conversions from [[utility.geometry.Vector2D]] to [[utility.geometry.Vector3D]] and vice versa. */
    implicit def vec3DToVec2D(value: Vector3D): Vector2D = Vector2D(value.x / value.z, value.y / value.z)

    implicit def vec2DToVec3D(value: Vector2D): Vector3D = Vector3D(value.x, value.y, 1.0)

}
