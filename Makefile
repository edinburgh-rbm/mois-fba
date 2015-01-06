JAVA_HOME?=/usr/lib/jvm/java-7-openjdk-amd64/

GLPK_VERSION=4.55
GLPK_JAVA_VERSION=1.0.37

GLPK=glpk-${GLPK_VERSION}
GLPK_DIST=http://ftp.gnu.org/gnu/glpk/${GLPK}.tar.gz

GLPK_JAVA=libglpk-java-${GLPK_JAVA_VERSION}
GLPK_JAVA_DIST=http://kent.dl.sourceforge.net/project/glpk-java/glpk-java/glpk-java-${GLPK_JAVA_VERSION}/${GLPK_JAVA}.tar.gz

SBT=sbt -batch -no-colors

TARGET_DIR=${CURDIR}/tmp

OBJECTS=${TARGET_DIR}/lib/libglpk.so \
	${TARGET_DIR}/lib/jni/libglpk_java.so \
	${TARGET_DIR}/share/java/glpk-java.jar

all: install_deps
	${SBT} compile

test:
	${SBT} test

install: all
	${SBT} publish-local

install_deps: install_glpk install_glpk_java
	mkdir -p lib
	cp ${OBJECTS} lib

configure_glpk: ${GLPK}/.configure_done
${GLPK}/.configure_done: ${GLPK}
	(cd ${GLPK} && ./configure \
			--prefix=${TARGET_DIR} \
			--with-gmp=yes \
			--with-pic=yes)
	touch $@

build_glpk: ${GLPK}/.build_done
${GLPK}/.build_done: ${GLPK}/.configure_done
	(cd ${GLPK} && make all)
	touch $@

install_glpk: ${GLPK}/.install_done
${GLPK}/.install_done: ${GLPK}/.build_done
	(cd ${GLPK} && make install)
	touch $@

configure_glpk_java: ${GLPK_JAVA}/.configure_done
${GLPK_JAVA}/.configure_done: ${GLPK_JAVA}
	(cd ${GLPK_JAVA} && ./configure \
				--prefix=${TARGET_DIR} \
				--enable-shared=yes \
				--enable-static=no \
				CPPFLAGS="-I${TARGET_DIR}/include -I/System/Library/Frameworks/JavaVM.framework/Headers" \
				LDFLAGS="-L${TARGET_DIR}/lib -I/System/Library/Frameworks/JavaVM.framework/Headers" \
				SWIGFLAGS="-I${TARGET_DIR}/include" \
				JAVA_HOME=${JAVA_HOME})
	touch $@

build_glpk_java: ${GLPK_JAVA}/.build_done
${GLPK_JAVA}/.build_done: ${GLPK_JAVA}/.configure_done
	(cd ${GLPK_JAVA} && make all)
	touch $@

install_glpk_java: ${GLPK_JAVA}/.install_done
${GLPK_JAVA}/.install_done: ${GLPK_JAVA}/.build_done
	(cd ${GLPK_JAVA} && make install)
	touch $@

${GLPK}: ${GLPK}.tar.gz
	sha1sum --quiet -c checksums/${GLPK}
	gzip -dc ${GLPK}.tar.gz | tar -xf -

${GLPK_JAVA}: ${GLPK_JAVA}.tar.gz
	sha1sum --quiet -c checksums/${GLPK_JAVA}
	gzip -dc ${GLPK_JAVA}.tar.gz | tar -xf -

${GLPK}.tar.gz:
	curl -Ls ${GLPK_DIST} > $@
${GLPK_JAVA}.tar.gz:
	echo ${GLPK_JAVA_DIST}
	curl -Ls ${GLPK_JAVA_DIST} > $@

clean:
	${SBT} clean
	rm -rf ${GLPK} ${GLPK}.tar.gz
	rm -rf ${GLPK_JAVA} ${GLPK_JAVA}.tar.gz
	rm -rf tmp
	rm -rf lib
