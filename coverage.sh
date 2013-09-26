#!/bin/sh
gnatmake -p -Ptests -XMODE=Coverage || exit $?
lcov --directory coverage/obj --zerocounters
coverage/bin/test_all >coverage.log
tail -n 4 coverage.log
lcov --gcov-tool gnatgcov --directory coverage/obj --output coverage/test-info.dat --capture || exit $?
genhtml --output-dir coverage coverage/test-info.dat
