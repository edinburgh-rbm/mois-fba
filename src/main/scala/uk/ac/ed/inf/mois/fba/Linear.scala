/*
 *  Linear Optimisation Process
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
package uk.ac.ed.inf.mois.fba

import scala.collection.mutable
import scala.language.implicitConversions
import scala.math.pow

import spire.algebra.{Order, Rig}

import no.uib.cipr.matrix.sparse.CompRowMatrix

import org.gnu.glpk.GLPK
import org.gnu.glpk.GLPKConstants
import org.gnu.glpk.GlpkException
import org.gnu.glpk.SWIGTYPE_p_double
import org.gnu.glpk.SWIGTYPE_p_int
import org.gnu.glpk.glp_prob
import org.gnu.glpk.glp_smcp

import uk.ac.ed.inf.mois.reaction.RateLawReactionNetwork
import uk.ac.ed.inf.mois.math.Multiset
import uk.ac.ed.inf.mois.Bounds

class LinOptProcess extends RateLawReactionNetwork[Double] {
  type Reaction = LinearNetwork

  class LinearNetwork(val lhs: Multiset[Species], val rhs: Multiset[Species])
      extends BaseReaction with Bounds[Double] {
    def at(k: => Double) = RateLawReaction(lhs, rhs, () => k)
  }

  object LinearNetwork {
    implicit class BoundSyntax(n: LinearNetwork)(implicit o: Order[Double], r: Rig[Double]) {
      def gte(b: Double) = { n.lowerBound = Some(new n.LowerBound(b)); n }
      def lte(b: Double) = { n.upperBound = Some(new n.UpperBound(b)); n }
      def nonnegative() = gte(r.zero)
    }
  }

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset[Species], rhs: Multiset[Species]) = new Reaction(lhs, rhs)
  }

  val rxns = mutable.ArrayBuffer.empty[LinearNetwork]
  /** specify the list of reactions in the network */
  def reactions(rss: Seq[LinearNetwork]*) =
    for (rs <- rss; r <- rs) rxns += r
  implicit def rxnToSeq(r: LinearNetwork) = Seq(r)

  // the objective function (multiset...)
  private var obj: Multiset[Species] = null
  /** define the objective function to minimise */
  def minimise(s: Multiset[Species]) {
    GLPK.glp_set_obj_dir(lp, GLPKConstants.GLP_MIN)
    obj = s
  }
  /** define the objective function to maximise */
  def maximise(s: Multiset[Species]) {
    GLPK.glp_set_obj_dir(lp, GLPKConstants.GLP_MAX)
    obj = s
  }

  /** (sparse) matrix of coefficients from the reactions */
  lazy val coefficientMatrix = {
    // FIXME: should be possible to do this in one pass, no?
    val nz = mutable.ArrayBuffer.empty[Array[Int]]
    for (r <- rxns) {
      val row = mutable.ArrayBuffer.empty[Int]
//      for (s <- (r.lhs + r.rhs).keys.toSeq.sortBy(_.meta)) {
      for (s <- r.lhs.keys ++ r.rhs.keys) {
        row += species.indexOf(s)
      }
      nz += row.toArray
    }
    val mat = new CompRowMatrix(rxns.size, species.size, nz.toArray)
    for (i <- 0 until rxns.size) {
      val r = rxns(i)
      for (s <- r.lhs.keys)
        mat.set(i, species.indexOf(s), r.lhs(s))
      for (s <- r.rhs.keys)
        mat.set(i, species.indexOf(s), -r.rhs(s))
    }
    mat
  }

  private lazy val coefficientCols: Array[Int] =
    coefficientMatrix.getColumnIndices map(_ + 1)

  private lazy val coefficientRows = {
    val coli: Array[Int] = coefficientMatrix.getColumnIndices // only size used
    val rptrs = coefficientMatrix.getRowPointers
    val ind = mutable.ArrayBuffer.empty[Int]

    var i = 1
    for (ri <- 1 until rptrs.size) {
      ind ++= Array.fill(rptrs(ri) - rptrs(ri-1))(i)
      i += 1
    }
    ind ++= Array.fill(coli.size - rptrs(rptrs.size-1))(i)
    ind.toArray
  }
  private lazy val coefficientData: Array[Double] = coefficientMatrix.getData


  private val lp = GLPK.glp_create_prob

  override def init(t: Double) {
    super.init(t)

    require (obj != null, "objective function is unspecified")

    GLPK.glp_set_prob_name(lp, toString)

    // set up rows, one for each reaction
    GLPK.glp_add_rows(lp, rxns.size)
    for (i <- 0 until rxns.size) {
      val r = rxns(i)
      GLPK.glp_set_row_name(lp, i+1, r.toString)
      if (r.lowerBound.isDefined)
        GLPK.glp_set_row_bnds(lp, i+1, GLPKConstants.GLP_LO, r.lowerBound.get.bound, 0)
      if (r.upperBound.isDefined)
        GLPK.glp_set_row_bnds(lp, i+1, GLPKConstants.GLP_UP, 0, r.upperBound.get.bound)
    }

    // set up columns, one for each reaction
    GLPK.glp_add_cols(lp, species.size)
    for (i <- 0 until species.size) {
      val s = species(i)
      GLPK.glp_set_col_name(lp, i+1, s.meta.identifier)
      if (s.lowerBound.isDefined)
        GLPK.glp_set_col_bnds(lp, i+1, GLPKConstants.GLP_LO, s.lowerBound.get.bound, 0)
      if (s.upperBound.isDefined)
        GLPK.glp_set_col_bnds(lp, i+1, GLPKConstants.GLP_UP, 0, s.upperBound.get.bound)
    }

    // set up objective function
    for((s, c) <- obj) {
      GLPK.glp_set_obj_coef(lp, species.indexOf(s)+1, c)
    }

    // helpers for loading the matrix of coefficients
    @inline def scala2swigI(a: Array[Int]): SWIGTYPE_p_int = {
      val si = GLPK.new_intArray(a.size + 1)
      for (i <- 0 until a.size) {
        GLPK.intArray_setitem(si, i+1, a(i))
      }
      si
    }
    @inline def scala2swigD(a: Array[Double]): SWIGTYPE_p_double = {
      val sd = GLPK.new_doubleArray(a.size + 1)
      for (i <- 0 until a.size) {
        GLPK.doubleArray_setitem(sd, i+1, a(i))
      }
      sd
    }

    // load the coefficient matrix
    val scr = scala2swigI(coefficientRows)
    val scc = scala2swigI(coefficientCols)
    val sdd = scala2swigD(coefficientData)
    GLPK.glp_load_matrix(lp, 1, scr, scc, sdd)
    GLPK.delete_intArray(scr)
    GLPK.delete_intArray(scc)
    GLPK.delete_doubleArray(sdd)

    // TODO: user constraints

    // and that's it. we set the constraints that come as
    // known values in the step function on the copy
  }

  override def step(t: Double, tau: Double) {
    val clp = GLPK.glp_create_prob
    GLPK.glp_copy_prob(clp, lp, GLPKConstants.GLP_OFF)
    GLPK.glp_simplex(clp, null)
    var i = 0
    while (i < species.size) {
      species(i) := GLPK.glp_get_col_prim(clp, i+1)
      i += 1
    }
    GLPK.glp_delete_prob(clp)
  }
}
