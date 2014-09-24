/*
 *  GLPK Lp problem test
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
import uk.ac.ed.inf.mois.fba

import org.gnu.glpk.GLPK
import org.gnu.glpk.GLPKConstants
import org.gnu.glpk.GlpkException
import org.gnu.glpk.SWIGTYPE_p_double
import org.gnu.glpk.SWIGTYPE_p_int
import org.gnu.glpk.glp_prob
import org.gnu.glpk.glp_smcp

/**
  * Transcribed from the libglpk-java examples
  *
  * Minimize
  *   z = -.5 * x1 + .5 * x2 - x3 + 1
  * subject to
  *   0.0 <= x1 - .5 * x2 <= 0.2
  *   -x2 + x3 <= 0.4
  * where,
  *   0.0 <= x1 <= 0.5
  *   0.0 <= x2 <= 0.5
  *   0.0 <= x3 <= 0.5
  */
class GlpkLpTest extends FlatSpec with Matchers {
  val precision = 1e-4
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "a sample LP problem" should "give a reasonable answer" in {
    val lp = GLPK.glp_create_prob
    GLPK.glp_set_prob_name(lp, "myProblem")

    // Define columns
    GLPK.glp_add_cols(lp, 3)
    GLPK.glp_set_col_name(lp, 1, "x1")
    GLPK.glp_set_col_kind(lp, 1, GLPKConstants.GLP_CV)
    GLPK.glp_set_col_bnds(lp, 1, GLPKConstants.GLP_DB, 0, 0.5)
    GLPK.glp_set_col_name(lp, 2, "x2")
    GLPK.glp_set_col_kind(lp, 2, GLPKConstants.GLP_CV)
    GLPK.glp_set_col_bnds(lp, 2, GLPKConstants.GLP_DB, 0, 0.5)
    GLPK.glp_set_col_name(lp, 3, "x3")
    GLPK.glp_set_col_kind(lp, 3, GLPKConstants.GLP_CV)
    GLPK.glp_set_col_bnds(lp, 3, GLPKConstants.GLP_DB, 0, 0.5)

    // Create constraints

    // Allocate memory
    val ind = GLPK.new_intArray(3)
    val value = GLPK.new_doubleArray(3)

    // Create rows
    GLPK.glp_add_rows(lp, 2)

    // Set row details
    GLPK.glp_set_row_name(lp, 1, "c1")
    GLPK.glp_set_row_bnds(lp, 1, GLPKConstants.GLP_DB, 0, 0.2)
    GLPK.intArray_setitem(ind, 1, 1)
    GLPK.intArray_setitem(ind, 2, 2)
    GLPK.doubleArray_setitem(value, 1, 1)
    GLPK.doubleArray_setitem(value, 2, -0.5)
    GLPK.glp_set_mat_row(lp, 1, 2, ind, value)

    GLPK.glp_set_row_name(lp, 2, "c2")
    GLPK.glp_set_row_bnds(lp, 2, GLPKConstants.GLP_UP, 0, 0.4)
    GLPK.intArray_setitem(ind, 1, 2)
    GLPK.intArray_setitem(ind, 2, 3)
    GLPK.doubleArray_setitem(value, 1, -1)
    GLPK.doubleArray_setitem(value, 2, 1)
    GLPK.glp_set_mat_row(lp, 2, 2, ind, value)

    // Free memory
    GLPK.delete_intArray(ind)
    GLPK.delete_doubleArray(value)

    // Define objective
    GLPK.glp_set_obj_name(lp, "z")
    GLPK.glp_set_obj_dir(lp, GLPKConstants.GLP_MIN)
    GLPK.glp_set_obj_coef(lp, 0, 1)
    GLPK.glp_set_obj_coef(lp, 1, -0.5)
    GLPK.glp_set_obj_coef(lp, 2, 0.5)
    GLPK.glp_set_obj_coef(lp, 3, -1)
    // Solve model
    val parm = new glp_smcp()
    GLPK.glp_init_smcp(parm)
    val ret = GLPK.glp_simplex(lp, parm)

    // Retrieve solution
    ret should equal (0)
    GLPK.glp_get_obj_val(lp) should equal (0.425)
    GLPK.glp_get_col_prim(lp, 1) should equal (0.25)
    GLPK.glp_get_col_prim(lp, 2) should equal (0.0999)
  }
}
