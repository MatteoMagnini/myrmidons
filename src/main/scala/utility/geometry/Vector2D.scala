package utility.geometry

  import Vectors._

/** A vector in 2-dimensional space.
    *
    * @param x x-coordinate
    * @param y y-coordinate
    */
  case class Vector2D(x: Double, y: Double) {

    import TupleOp2._

    /** Returns a vector in its opposite form */
    def - : Vector2D = (-x, -y)

    /** Returns a vector shifted of vector v */
    def >>(delta: Vector2D): Vector2D = (x + delta.x, y + delta.y)

    /** Returns a vector subtraction */
    def -(delta: Vector2D): Vector2D = this >> (delta -)

    /** Returns a vector multiplied by a constant */
    def *(s: Double): Vector2D = (x * s, y * s)

  /** Returns a vector multiplied by a constant */
    def /(s: Double): Vector2D = (x / s, y / s)

    /** Returns the norm of the vector */
    def || : Double = math.sqrt(x * x + y * y)

    /** Returns the angle of the vector */
    def /\ : Double = math.atan2(y, x)

    def ^(s: Vector2D): Double = {
      val numeratorCos = (s.x * this.x) + (s.y * this.y)
      val denominator = (this ||) * (s ||)
      math.acos(numeratorCos / denominator)
    }

    /** Return the distance between vectors */
    def -->(other: Vector2D): Double = (this - other) ||

    /** Returns vectors equality using a default precision value */
    def ~~(other: Vector2D, precision: Double = 3): Boolean = ~=(x, other.x, precision) && ~=(y, other.y, precision)

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
    def apply(radiant: Double, module: Double): Vector2D =
      Vector2D(math.cos(radiant) * module, math.sin(radiant) * module)
  }

  /** Oriented vector factory */
  object OrientedVector2DWithNoise {
    def apply(radiant: Double, module: Double, noise: Double): Vector2D = {
      assert(noise <= 1 && noise >= 0)
      val newRadiant = doubleInRange(radiant - noise * radiant, radiant + noise * radiant)
      OrientedVector2D(newRadiant, doubleInRange(module * (1 - noise), module))
    }
  }

/** Random vector created in a circle area factory */
  object RandomVector2DInCircle {
    def apply(minRadius: Double = 0.0, maxRadius: Double = 1.0, center: Vector2D = ZeroVector2D()): Vector2D = {
      assert(minRadius >= 0)
      assert(maxRadius > 0)
      OrientedVector2D(scala.util.Random.nextDouble() * 2 * Math.PI, doubleInRange(minRadius, maxRadius)) >> center
    }

  }

  /** Random vector factory inside a square or rectangle */
  object RandomVector2DInSquare {
    import TupleOp2._
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
      (x, y)
    }

    /**
      * Similar to the constructor without perturbation.
      *
      * @param min max value of a dimension
      * @param max min value of a dimension
      * @param perturbation a vector to influence the uniform distribution inside the square
      * @return a vector inside the square
      */
    def apply(min: Double, max: Double, perturbation: Vector2D): Vector2D = {
      def bound(min: Double, max: Double, v: Vector2D): Vector2D =
      Vector2D(if (v.x < min) min else if (v.x > max) max else v.x,
        if (v.y < min) min else if (v.y > max) max else v.y)

      bound(min, max, apply(min, max) >> perturbation)
    }


    /** Create a random vector in a rectangle.
      *
      * @param minX for first dimension
      * @param maxX for first dimension
      * @param minY for second dimension
      * @param maxY for second dimension
      * @return a vector inside the rectangle with uniform distribution
      */
    def apply(minX: Double, maxX: Double, minY: Double, maxY: Double): Vector2D =
      Vector2D(doubleInRange(minX, maxX), doubleInRange(minY, maxY))
  }


  /** Implicit conversions for [[utility.geometry.Vector2D]] instances
    *
    * {{{
    * import TupleOp._
    * val v = (1, 2) >> (3, 4)
    * }}}
    */
  object TupleOp2 {
    implicit def toVec2D(value: (Double, Double)): Vector2D = Vector2D(value._1, value._2)

    implicit def intToVec2D(value: (Int, Int)): Vector2D = Vector2D(value._1.toDouble, value._2.toDouble)
  }


