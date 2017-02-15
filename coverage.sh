#!/bin/sh

: ${GNATPATH:=/usr/local/gcc-aux/bin}
: ${LCOV_DATA:=coverage/test-info.dat}
: ${LCOV_HTML_DIR:=coverage}
: ${TEST_LOG:=coverage.log}

PATH=${GNATPATH}:${PATH} \
gprbuild -XTASK_SAFETY=Intel -p -Ptests -XMODE=Coverage || exit $?
PATH=${GNATPATH}:${PATH} \
gprbuild -XTASK_SAFETY=Intel -p -Ptools -XMODE=Coverage || exit $?
lcov --directory coverage/obj --zerocounters
coverage/bin/test_all > "${TEST_LOG}"
tools/tests.sh coverage/bin
tail -n 4  "${TEST_LOG}"
lcov --gcov-tool ${GNATPATH}/gcov --directory coverage/obj --output "${LCOV_DATA}" --capture || exit $?
genhtml --output-dir "${LCOV_HTML_DIR}" "${LCOV_DATA}"
