#!/bin/sh
gnatmake -p -Ptests -XMODE=Coverage || exit $?
coverage/bin/test_all
lcov --gcov-tool gnatgcov --directory coverage/obj --output coverage/test-info.dat --capture || exit $?
genhtml --output-dir coverage coverage/test-info.dat
