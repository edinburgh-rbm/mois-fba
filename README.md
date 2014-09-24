Flux Balance Analysis Process-type for MOIS
===========================================

Installation and use is a little bit different from the
rest of MOIS. This is because we have to use a shared 
library written in C.

The procedure for installing is, at the moment, to clone
the git repository and build using Make:

~~~~~
git clone https://github.com/edinburgh-rbm/mois-fba
cd mois-fba
make all
make test
make install
~~~~~

There are prerequisites. Amongst them,

  * gcc
  * libtool
  * swig
  * a jdk
  * the gmp library

The most likely error is that the jni.h header file is not
found. In this case, be sure to set JAVA_HOME in the 
environment.
