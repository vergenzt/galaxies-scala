package com.github.vergenzt

import scala.util.Try
import scala.collection.mutable

package object galaxies {

  type Vec = Array[Int]
  implicit class VecOps(u: Vec) {
    def +(v: Vec) = (u zip v).map({ case (ui,vi) => ui + vi })
    def -(v: Vec) = (u zip v).map({ case (ui,vi) => ui - vi })
    def *(n: Int) = u.map(_ * n)
    def mirror(g: Vec): Vec = {
      g - (u - g)
    }
    def isCenter: Boolean = u.forall(_ % 2 == 0)
  }

  def iterateFromOrigin(bounds: Vec) = {
    var vec = Array.fill(bounds.length)(0)
    Iterator.fill(bounds.product) {
      var dim = 0
      while (vec(dim) == bounds(dim) - 1) {
        vec(dim) += 1
        vec(dim) %= bounds(dim)
        dim += 1
      }
      vec
    }
  }

  /**
   * A Galaxy is defined by an integer name and the address of its center.
   * Even-parity addresses are center-aligned within a cell, odd-parity
   * addresses straddle the line between two cells.
   *
   * Each dimension has 2n addresses where n is the number of cells in that
   * dimension: addresses 0, 2, ..., 2n-2 align with cell centers, while
   * addresses 1, 3, ..., 2n-1 align with cell borders.
   */
  type Galaxy = Vec

  type Size = Vec
  type Name = Int

  /**
   * A GalaxiesSolver takes a vector for the number of cells in each dimension,
   * along with a sequence of Galaxies. Galaxies are provided in sorted order
   * starting with the last dimension. (i.e. sorted first by y, then by x.)
   *
   * Return value should be a map from even-parity vectors (i.e. cell centers)
   * to galaxy indices.
   */
  type GalaxiesSolver = (Size, Map[Name, Vec]) => Map[Vec, Name]

  /**
   * Error thrown for invalid solutions.
   */
  case class SolutionNotValid(message: String) extends RuntimeException

  /**
   * Verifies that the provided result is valid for the given galaxies.
   */
  def verifySolution(size: Vec, galaxies: Map[Name, Vec], solver: GalaxiesSolver): Try[Unit] = Try {
    val result: Map[Vec, Name] = solver.apply(size, galaxies)

    iterateFromOrigin(size)
      .filter(_.forall(_ % 2 == 0))
      .foreach { cell =>
        if (!result.contains(cell)) {
          throw SolutionNotValid(s"missing association for cell: $cell")
        }
      }

    result.foreach { case (cell, galaxyName) =>
      val galaxy = galaxies(galaxyName)

      if (!cell.isCenter) {
        throw SolutionNotValid(s"address is not a cell center: $cell")
      }

      val withinBounds = cell.zip(size).forall({ case (value, bound) => 0 <= value && value < bound })
      if (!withinBounds) {
        throw SolutionNotValid(s"address is not within bounds: $cell")
      }

      val mirrorImage = cell.mirror(galaxy)
      if (result.get(mirrorImage) != Some(galaxyName)) {
        throw SolutionNotValid(s"galaxy $galaxyName does not own mirror of cell $cell")
      }
    }
  }

  /// begin implementation ///

  class myGalaxySolver extends GalaxiesSolver {
    def apply(size: Vec, galaxies: Map[Name, Vec]): Map[Vec, Int] = {

      val result = mutable.Map.empty[Vec, mutable.Set[Int]]
        .withDefault({
          // build once
          val names = mutable.BitSet.empty ++ galaxies.keys
          // clone for each entry that needs a default
          (_) => names.clone()
        })

      def markAssociated(galaxy: (Name, Vec))(vec: Vec) = {
        result(vec) = mutable.BitSet(galaxy._1)
      }

      // expand all odd-parity galaxies
      galaxies.foreach {
        case galaxy @ (name, vec) =>
          if (!vec.isCenter) {
            val cellsToExpand = vec
              .map {
                case xi if xi % 2 == 0 => Array(xi, xi)
                case xi if xi % 2 == 1 => Array(xi - 1, xi + 1)
              }
              .transpose

            cellsToExpand.foreach(markAssociated(galaxy))
          }
      }

      ???
    }
  }

  object Runner extends App {
    val size = (7,7)

    val galaxies: Map[Name, Vec] = {
      val galaxies: Seq[Vec] = Seq(
        Array(2, 1)
      )
      galaxies.zipWithIndex.map(_.swap).toMap
    }

  }
}
