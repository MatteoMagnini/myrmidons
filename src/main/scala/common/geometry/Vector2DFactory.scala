package common.geometry

import common.geometry.Vectors.doubleInRange

object Vector2DFactory {

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
      OrientedVector2D(scala.util.Random.nextDouble() * 2 * math.Pi, doubleInRange(minRadius, maxRadius)) >> center
    }

    def apply(minMaxRadius: (Double, Double), center: Vector2D): Vector2D = {
      RandomVector2DInCircle(minMaxRadius._1, minMaxRadius._2, center)
    }
  }

  /** Random vector factory inside a square or rectangle */
  object RandomVector2DInSquare {
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

    /** Constructor without perturbation.
     *
     * @param min          max value of a dimension
     * @param max          min value of a dimension
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

}
