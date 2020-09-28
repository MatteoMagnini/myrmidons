package utility

import utility.Geometry.Vector2D

object Geometry {

  /**
   * Double equivalence check.
   *
   * @param x default value
   * @param y value to check
   * @param precision to consider in number matching
   *
   * @return true if the value are similar, otherwise return false
   * */
  def ~=(x: Double, y: Double, precision: Double = 1E-1): Boolean = (x - y).abs < precision

  /** A vector in 2-dimensional space.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   */
  case class Vector2D(x: Double, y: Double) {
    import TupleOp._

    /** Returns a vector in its opposite form */
    def - : Vector2D = (-x, -y)

    /** Returns a vector shifted of vector v */
    def >> (delta: Vector2D): Vector2D = (x + delta.x, y + delta.y)

    /** Returns a vector subtraction */
    def - (delta: Vector2D): Vector2D = this >> (delta -)

    /** Returns a vector multiplied by a constant */
    def * (s: Double): Vector2D = (s * x, s * y)

    /** Returns the norm of the vector */
    def || : Double = math.sqrt(x * x + y * y)

    /** Returns the angle of the vector */
    def /\ : Double = math.atan2(y,x)

    def ^ (s: Vector2D) : Double = {
      val numeratorCos = (s.x * this.x) + (s.y * this.y)
      val denominator = (this ||) * (s ||)
      math.acos(numeratorCos / denominator)

    }

    /** Return the distance between vectors */
    def -->(other: Vector2D) : Double = this - other ||

    def ~~(other: Vector2D, precision: Double = 3): Boolean = ~=(x, other.x,precision) && ~=(y, other.y,precision)


    override def equals(obj: Any): Boolean = obj match {
      case o: Vector2D => ~=(x, o.x) && ~=(y, o.y)
      case _ => false
    }

  }

  /** Vector factory */
  object Vector2D {
    def apply(x: Double, y: Double): Vector2D = new Vector2D(x, y)
  }

  /** Zero vector factory */
  object ZeroVector2D {
    def apply(): Vector2D = Vector2D(0, 0)
  }

  /** Oriented vector factory */
  object OrientedVector2D {
    def apply(radiant: Double, module: Double ): Vector2D =
      Vector2D(math.cos(radiant) * module, math.sin(radiant) * module)
  }

  /** Oriented vector factory */
  object OrientedVector2DWithNoise {
    def apply(radiant: Double, module: Double, noise: Double ): Vector2D = {
      assert(noise <= 1 && noise >= 0)
      val newRadiant = doubleInRange(radiant - noise * radiant, radiant + noise *radiant)
      OrientedVector2D(newRadiant, doubleInRange(module * (1 - noise), module))
    }
  }

  object RandomVector2DInCircle {

    def apply( minRadius: Double = 0.0, maxRadius: Double = 1.0, center: Vector2D = ZeroVector2D()): Vector2D = {
      assert(minRadius >= 0)
      assert(maxRadius > 0)
      OrientedVector2D(scala.util.Random.nextDouble() * 2 * Math.PI, doubleInRange(minRadius, maxRadius)) >> center
    }

  }

  /** Random vector factory inside a square or rectangle*/
  object RandomVector2DInSquare {
    import TupleOp._

    /**
     * @param min value of a dimension
     * @param max value of a dimension
     * @return a vector inside the square with uniform distribution
     *         A  B
     *         C  D
     *         where:
     *         A = (min,max),
     *         B = (max,max),
     *         C = (min,min),
     *         D = (max,min)
     */
    def apply(min: Double, max: Double): Vector2D = {
      val x = doubleInRange(min, max)
      val y = doubleInRange(min, max)
      (x,y)
    }

    /**
     * Similar as the constructor without perturbation.
     * @param min value of a dimension
     * @param max value of a dimension
     * @param perturbation a vector to influence the uniform distribution inside the square
     * @return a vector inside the square
     */
    def apply(min: Double, max: Double, perturbation: Vector2D): Vector2D =
      bound(min, max, apply(min, max) >> perturbation)

    /**
     * Create a random vector in a rectangle.
     * @param minX for first dimension
     * @param maxX for first dimension
     * @param minY for second dimension
     * @param maxY for second dimension
     * @return a vector inside the rectangle with uniform distribution
     */
    def apply(minX: Double, maxX: Double, minY: Double, maxY: Double ): Vector2D =
      Vector2D(doubleInRange(minX, maxX), doubleInRange(minY, maxY))
  }

  private def bound(min: Double, max: Double, v: Vector2D): Vector2D =
    Vector2D(if (v.x < min) min else if (v.x > max) max else v.x,
      if (v.y < min) min else if (v.y > max) max else v.y)

  def doubleInRange(min: Double, max: Double): Double =
    min + (max - min) * scala.util.Random.nextDouble()

  /** Implicit conversions for [[utility.Geometry.Vector2D]] instances
   *
   * {{{
   * import TupleOp._
   * val v = (1, 2) >> (3, 4)
   * }}}
   */
  object TupleOp {
    implicit def toVec2D(value: (Double, Double)): Vector2D = Vector2D(value._1, value._2)
    implicit def intToVec2D(value: (Int, Int)): Vector2D = Vector2D(value._1.toDouble, value._2.toDouble)
    implicit def vec3DToVec2D(value: Vector3D): Vector2D = Vector2D(value.x / value.z, value.y / value.z)
  }


 /** A vector in 3-dimensional space.
    *
    * @param x x-coordinate
    * @param y y-coordinate
    * @param z z-coordinate
    */
  case class Vector3D(x: Double, y: Double, z: Double) {

   import TupleOp3._
   def - : Vector3D = (-x, -y, -z)

   def >>(delta: Vector3D): Vector3D = (x + delta.x, y + delta.y, z + delta.z)

   def -(delta: Vector3D): Vector3D = (x - delta.x, y - delta.y, z - delta.z) //this >> (delta -)

   def *(s: Double): Vector3D = (s * x, s * y, s * z)

   def /(s: Double): Vector3D = (x / s, y / s, z / s)

   def || : Double = math.sqrt(x * x + y * y + z * z)

   def /\ : Double = math.atan(y / x)

   def ^ (s: Vector3D) : Double = {
     val numeratorCos = (s.x * this.x) + (s.y * this.y) + (s.z * this.z)
     val denominator = (this ||) * (s ||)
     math.acos(numeratorCos / denominator)
   }

   def -->(other: Vector3D): Double = this - other ||

   /**
    * the distance between start point and intersection, summed distance to
    * intersection and stop point must be equals to distance between start point
    * and stop point.
    *
    * @param start first point
    * @param stop second point
    *
    * @return true if this vector is inside start and stop vector, otherwise return false
    *
    * */
   def checkInside(start: Vector3D, stop: Vector3D): Boolean = {
    ~=((start --> this) + (stop --> this) - (start --> stop), 0.0, 1E-6)
   }


   /** Cross product between two vectors * */
   def X(other: Vector3D): Vector3D = {
     val x = (this.y * other.z) - (this.z * other.y)
     val y = (this.z * other.x) - (this.x * other.z)
     val z = (this.x * other.y) - (this.y * other.x)
     (x, y, z)
   }
 }


   /** Implicit conversion to convert [[utility.Geometry.Vector3D]] instances
    *
    * {{{
    * import TupleOp3._
    * val v = (1, 2, 1) >> (3, 4, 1)
    * }}}
     * */
   object TupleOp3 {
     implicit def toVec3D(value: (Double, Double, Double)): Vector3D = Vector3D(value._1, value._2, value._3)
     implicit def intToVec3D(value: (Int, Int, Int)): Vector3D = Vector3D(value._1.toDouble, value._2.toDouble, value._3.toDouble)
     implicit def vec2DToVec3D(value: Vector2D): Vector3D = Vector3D(value.x, value.y, 1.0)
   }
}

object Prova extends App {
  val v1 = Vector2D(3.000,3.000)
  val v3 = Vector2D(3.0001,3.0001)
  val v2 = (3,3)
  import utility.Geometry.TupleOp._
  print(v2.equals(v1))
}