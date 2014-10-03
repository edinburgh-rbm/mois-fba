/*
 *  Linear Reaction Network problem test
 *  Copyright (C) 2014 University of Edinburgh
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics
import uk.ac.ed.inf.mois.fba.LinOptProcess
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

class ExampleLinearProblem extends LinOptProcess {
  val x1 = Species("x1"); x1 nonnegative()
  val x2 = Species("x2"); x2 nonnegative()
  val x3 = Species("x3"); x3 nonnegative()

  def objective = x1 * 10 + x2 * 6 + x3 * 4
  maximise

  reactions(
    x1 --> 1(x2) + 1(x3) lte(100), // XXX FIXME -1
    10(x1) --> 4(x2) + 5(x3) lte(600),
    2(x1) --> 2(x2) + 6(x3) lte(300)
  )

}

class LinearTest extends FlatSpec with Matchers {
  val precision = 1e-4
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  val elp = new ExampleLinearProblem
  elp.init(0)

  "reactions" should "be convertible to matrices" in {
    elp.x1 := 50.0
    println(elp.coefficientMatrix)
    elp.step(0, 1)
  }
}
