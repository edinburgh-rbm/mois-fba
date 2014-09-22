/*
 *  Flux-Balance Analysis for Scala
 *  Copyright (C) 2014 University of Edinburgh
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

// adapted from
// https://github.com/fommil/jniloader/blob/master/src/main/java/com/github/fommil/jni/JniNamer.java
object OS {
  def arch: String = {
    val arch = System.getProperty("os.arch", "").toLowerCase()
    if (arch.equals("x86") || arch.equals("i386") || arch.equals("i486") ||
        arch.equals("i586") || arch.equals("i686"))
      "i686"
    else if (arch.equals("x86_64") || arch.equals("amd64"))
      "x86_64"
    else if (arch.equals("ia64"))
      "ia64"
    else if (arch.equals("arm"))
      "arm"
    else if (arch.equals("armv5l"))
      "armv5l"
    else if (arch.equals("armv6l"))
      "armv6l"
    else if (arch.equals("armv7l"))
      "armv7l"
    else if (arch.equals("sparc"))
      "sparc"
    else if (arch.equals("sparcv9"))
      "sparcv9"
    else if (arch.equals("pa_risc2.0"))
      "risc2"
    else if (arch.equals("ppc"))
      "ppc"
    else if (arch.startsWith("ppc"))
      "ppc64"
    else
      "unknown"
  }

  // alternative: https://github.com/sgothel/gluegen/blob/master/src/java/jogamp/common/os/PlatformPropsImpl.java#L211
  private def isArmHf: Boolean = {
    val props = Seq("sun.boot.library.path", "java.library.path", "java.home")
    val paths = Seq("/lib/arm-linux-gnueabihf", "/usr/lib/arm-linux-gnueabihf")
    val searched = paths.filter(
      System.getProperty(_, "").matches(".*(gnueabihf|armhf).*")
    ) ++ paths.filter(new java.io.File(_).exists)
    !searched.isEmpty
  }

  def abi(arch: String): String = {
    if (!arch.startsWith("arm"))
      ""
    else try {
      if (isArmHf)
        "hf"
      else
        ""
    } catch {
      case e: Exception => "unknown"
    }
  }

  def os: String = {
    val os = System.getProperty("os.name", "").toLowerCase()
    if (os.startsWith("linux"))
      "linux"
    else if (os.startsWith("windows"))
      "win"
    else if (os.startsWith("mac os x") || os.startsWith("darwin"))
      "osx"
    else if (os.startsWith("freebsd"))
      "freebsd"
    else if (os.startsWith("android"))
      "android"
    else if (os.startsWith("sunos"))
      "sun"
    else if (os.startsWith("hp-ux"))
      "hpux"
    else if (os.startsWith("kd"))
      "kd"
    else
      "unknown"
  }
}
