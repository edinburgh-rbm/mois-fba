/*
 *  Flux Balance Analysis for Scala
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
import sbt._
import Keys._

object FbaBuild extends Build {
  lazy val root = Project(
    id="root",
    base=file("."),
    settings=
      Defaults.defaultSettings ++
        Seq(
          name := "mois-fba",
          organization := "uk.ac.ed.inf",
          version := "0.1-SNAPSHOT",
          scalaVersion := "2.11.2",
          scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
          resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
          resolvers += "ucar-unidata-releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/",
          libraryDependencies += "uk.ac.ed.inf" %% "mois" % "1.99.6-SNAPSHOT" exclude("xml-apis", "xml-apis"),
          libraryDependencies += "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.1",
          libraryDependencies += "com.github.fommil.netlib" % "all" % "1.1.2",
          libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test",
          unmanagedJars in Compile := {
            val jars = (baseDirectory.value / "lib") * ("glpk-java.jar")
            jars.classpath
          },
          exportJars := true,
          crossPaths := true,
          resourceGenerators in Compile += task[Seq[File]] {
            val sofiles = (baseDirectory.value / "lib") ** "*.so"
            sofiles.get
          }
        )
  )
}
