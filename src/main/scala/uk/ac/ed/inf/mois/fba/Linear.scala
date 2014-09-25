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

class LinOptProcess extends RateLawReactionNetwork[Double] {
  type Reaction = LinearNetwork

  class LinearNetwork(val lhs: Multiset[Species], val rhs: Multiset[Species])
      extends BaseReaction {
    def at(k: => Double) = RateLawReaction(lhs, rhs, () => k)
  }

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset[Species], rhs: Multiset[Species]) = new Reaction(lhs, rhs)
  }

  val rxns = mutable.ArrayBuffer.empty[LinearNetwork]
  def reactions(rss: Seq[LinearNetwork]*) =
    for (rs <- rss; r <- rs) rxns += r
  implicit def rxnToSeq(r: LinearNetwork) = Seq(r)

  lazy val coefficientMatrix = {
    // FIXME: should be possible to do this in one pass, no?
    val nz = mutable.ArrayBuffer.empty[Array[Int]]
    for (r <- rxns) {
      val row = mutable.ArrayBuffer.empty[Int]
      for (s <- (r.lhs + r.rhs).keys.toSeq.sortBy(_.meta)) {
        row += species.indexOf(s)
      }
      nz += row.toArray
    }
    val mat = new CompRowMatrix(rxns.size, species.size, nz.toArray)
    for (i <- 0 until rxns.size) {
      val r = rxns(i)
      val specs = r.lhs + r.rhs
      for (s <- specs.keys.toSeq.sortBy(_.meta)) {
        mat.set(i, species.indexOf(s), specs(s))
      }
    }
    mat
  }

  def coefficientCols: Array[Int] =
    coefficientMatrix.getColumnIndices map(_ + 1)

  def coefficientRows = {
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
  def coefficientData: Array[Double] = coefficientMatrix.getData

  private val lp = GLPK.glp_create_prob
  override def init(t: Double) {
    super.init(t)
    GLPK.glp_set_prob_name(lp, toString)
    GLPK.glp_add_rows(lp, rxns.size)
    for (i <- 0 until rxns.size) {
      GLPK.glp_set_row_name(lp, i+1, rxns(i).toString)
    }
    GLPK.glp_add_cols(lp, species.size)
    for (i <- 0 until species.size) {
      GLPK.glp_set_col_name(lp, i+1, species(i).meta.identifier)
    }

    loadMatrix
  }

  private def loadMatrix {
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

    val scr = scala2swigI(coefficientRows)
    val scc = scala2swigI(coefficientCols)
    val sdd = scala2swigD(coefficientData)
    GLPK.glp_load_matrix(lp, 1, scr, scc, sdd)
    GLPK.delete_intArray(scr)
    GLPK.delete_intArray(scc)
    GLPK.delete_doubleArray(sdd)
  }
}
