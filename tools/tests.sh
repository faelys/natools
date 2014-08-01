#!/bin/sh

: ${BIN_DIR:=${1:-$(dirname $0)}}
: ${TMP_TEMPLATE:=/tmp/tmp.XXXXXX}

RET_VAL="0"

if ! test -x ${BIN_DIR}/hmac-md5 \
   -o -x ${BIN_DIR}/hmac-sha1 \
   -o -x ${BIN_DIR}/hmac-sha256 \
   -o -x ${BIN_DIR}/sxcat
then
	echo "Unable to find tested executables in ${BIN_DIR}, aborting" >&2
	exit 2
fi


####################
# Test of hmac-md5 #
####################

if test -x ${BIN_DIR}/hmac-md5; then
	OUTPUT=$("${BIN_DIR}"/hmac-md5 '' '')
	if test "${OUTPUT}" != "74e6f7298a9c2d168935f58c001bad88"; then
		echo "Wrong output for HMAC-MD5('', ''): ${OUTPUT}"
		RET_VAL="1"
	fi;

	MSG="what do ya want for nothing?"
	OUTPUT=$(printf "Jefe\n${MSG}" \
	    | "${BIN_DIR}"/hmac-md5 -r -f - \
	    | xxd -p)
	if test "${OUTPUT}" != "750c783e6ab0b503eaa86e310a5db738"; then
		echo "Wrong output for HMAC-MD5('Jefe', '${MSG}'): ${OUTPUT}"
		RET_VAL="1"
	fi;
else
	echo "${BIN_DIR}/hmac-md5 not found"
fi


#####################
# Test of hmac-sha1 #
#####################

if test -x ${BIN_DIR}/hmac-sha1; then
	OUTPUT=$("${BIN_DIR}"/hmac-sha1 -H '' '')
	if test "${OUTPUT}" != "FBDB1D1B18AA6C08324B7D64B71FB76370690E1D"; then
		echo "Wrong output for HMAC-SHA1('', ''): ${OUTPUT}"
		RET_VAL="1"
	fi;

	MSG="The quick brown fox jumps over the lazy dog"
	OUTPUT=$(echo -n "${MSG}" \
	    | "${BIN_DIR}"/hmac-sha1 -b "key")
	if test "${OUTPUT}" != "3nybhbi3iqa8ino29wqQcBydtNk="; then
		echo "Wrong output for HMAC-SHA1('key', '${MSG}'): ${OUTPUT}"
		RET_VAL="1"
	fi;
else
	echo "${BIN_DIR}/hmac-sha1 not found"
fi


#######################
# Test of hmac-sha256 #
#######################

if test -x ${BIN_DIR}/hmac-sha256; then
	TMP_FILE=$(mktemp "${TMP_TEMPLATE}")
	OUTPUT=$("${BIN_DIR}"/hmac-sha256 -h -f "${TMP_FILE}" '')
	if test "${OUTPUT}" != "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"; then
		echo "Wrong output for HMAC-SHA256('', ''): ${OUTPUT}"
		RET_VAL="1"
	fi;

	echo 0102030405060708090a0b0c0d0e0f10111213141516171819 \
	    | xxd -r -p \
	    >"${TMP_FILE}"

	OUTPUT=$(echo cdcdcdcdcdcdcdcdcdcd cdcdcdcdcdcdcdcdcdcd \
	              cdcdcdcdcdcdcdcdcdcd cdcdcdcdcdcdcdcdcdcd \
	              cdcdcdcdcdcdcdcdcdcd \
	    | xxd -r -p \
	    | "${BIN_DIR}"/hmac-sha256 -b -f "${TMP_FILE}")
	if test "${OUTPUT}" != "glWKOJpEPA6kzIGYmfIIOoXw+qPlePgHei4/9GcpZls="; then
		echo "Wrong output for HMAC-SHA256(...): ${OUTPUT}"
		RET_VAL="1"
	fi;
	rm "${TMP_FILE}"
else
	echo "${BIN_DIR}/hmac-sha256 not found"
fi


#################
# Test of sxcat #
#################

if test -x ${BIN_DIR}/sxcat; then
	TMP_SRC1=$(mktemp "${TMP_TEMPLATE}")
	TMP_SRC2=$(mktemp "${TMP_TEMPLATE}")
	TMP_DEST=$(mktemp "${TMP_TEMPLATE}")
	TMP_EXPT=$(mktemp "${TMP_TEMPLATE}")

	${BIN_DIR}/sxcat -h >"${TMP_EXPT}"

	if ${BIN_DIR}/sxcat --indent mew >/dev/null 2>/dev/null \
	    || ${BIN_DIR}/sxcat --indent 3u >/dev/null 2>/dev/null \
	    || ${BIN_DIR}/sxcat -i 999999999999999999999 >/dev/null 2>/dev/null
	then
		echo "sxcat: unexpected success with invalid indentation"
		RET_VAL="1"
	fi

	if ${BIN_DIR}/sxcat --nl-encoding gl-hf >/dev/null 2>/dev/null \
	    || ${BIN_DIR}/sxcat --nl-encoding - >/dev/null 2>/dev/null
	then
		echo "sxcat: unexpected success with invalid newline encoding"
		RET_VAL="1"
	fi

	if ${BIN_DIR}/sxcat --width mu 2>"${TMP_DEST}" \
	    || ${BIN_DIR}/sxcat --tab-stop '' >/dev/null 2>/dev/null
	then
		echo "sxcat: unexpected success with invalid natural argument"
		RET_VAL="1"
	fi

	if ! diff -u "${TMP_EXPT}" "${TMP_DEST}"; then
		echo "sxcat: unexpected errror output"
		RET_VAL="1"
	fi

	echo "The lazy fox" >"${TMP_SRC1}"
	echo 0102030405060708 | xxd -r -p >"${TMP_SRC2}"
#	echo -n '"Test""The lazy fox\n"#0102030405060708#' >"${TMP_EXPT}"

	echo -n "13:" >"${TMP_EXPT}"
	cat "${TMP_SRC1}" >>"${TMP_EXPT}"
	echo -n "4:Test8:" >>"${TMP_EXPT}"
	cat "${TMP_SRC2}" >>"${TMP_EXPT}"

	if ! echo -n Test \
	    | ${BIN_DIR}/sxcat --atom --canonical \
	      "${TMP_SRC1}" - "${TMP_SRC2}" \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat atom test 1 failed"
		RET_VAL="1"
	fi

	echo -n "|VGhlIGxhenkgZm94Cg==|" >"${TMP_EXPT}"
	if ! ${BIN_DIR}/sxcat --atom --canonical --base64 "${TMP_SRC1}" \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat atom test 2 failed"
		RET_VAL="1"
	fi

	dd if=/dev/zero of="${TMP_SRC2}" bs=1024 count=4 2>/dev/null
	echo -n '4096:' >"${TMP_EXPT}"
	cat "${TMP_SRC2}" >> "${TMP_EXPT}"
	if ! ${BIN_DIR}/sxcat --atom --canonical "${TMP_SRC2}" \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat atom test 3 failed"
		RET_VAL="1"
	fi

	echo -n "{KDc6c25pY2tlcjM6YWJjKDE6AzM6YWJjKSk=}" >"${TMP_EXPT}"
	if ! echo '(snicker "abc" (#03# |YWJj|))' \
	    | ${BIN_DIR}/sxcat --canonical --brace \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat basic expression test failed"
		RET_VAL="1"
	fi

	printf '("test"\r\n   ("mang\\303\\251"))' >"${TMP_EXPT}"
	if ! echo '(test (mangé))' \
	    | ${BIN_DIR}/sxcat --canonical --width 0 --indent 3s --ASCII \
	      --single-line --dos --newline ao --escape-octal \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat encoding test failed"
		RET_VAL="1"
	fi

	printf '("rosé"x\n\r   x x x\n\r   x x)' >"${TMP_EXPT}"
	if ! echo '(rosé x x x x x x)' \
	    | ${BIN_DIR}/sxcat --canonical --token --single-line --width 8 \
	       --utf-8 --indent 3s --nl lfcr \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat UTF-8 width test failed"
		RET_VAL="1"
	fi

	printf '("rosé"\r\tx x x\r\tx x x\r)' >"${TMP_EXPT}"
	if ! echo '(rosé x x x x x x)' \
	    | ${BIN_DIR}/sxcat --canonical --token --single-line --width 8 \
	       --8-bit --indent 1t --tab-stop 3 --nl cR \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat latin-1 width test failed"
		RET_VAL="1"
	fi

	${BIN_DIR}/sxcat --ext-token --indent 3st --hex-atom --escape-hex \
	    --no-quoted --no-token --newline ')d,)(,oo,d(' --space 'dd' \
	    --width 80 --nl cr-lf --unix --lower --dump >"${TMP_DEST}"

	if ! ${BIN_DIR}/sxcat --config "${TMP_DEST}" --dump \
	    | diff -u "${TMP_DEST}" -
	then
		echo "sxcat configuration reload test failed"
		RET_VAL="1"
	fi

	printf '(#666f6f#\n   (# 626172# # 62617a#))' >"${TMP_EXPT}"
	if ! echo '(foo (bar baz))' \
	    | ${BIN_DIR}/sxcat --canonical --config "${TMP_DEST}" \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat configuration file test failed"
		RET_VAL="1"
	fi

	printf '("hello" "0123456789" "tab\\tencoding" 2:é)' >"${TMP_EXPT}"
	if ! echo '(hello 10:0123456789 "tab	encoding" é)' \
	    | ${BIN_DIR}/sxcat --config "${TMP_DEST}" --ASCII --no-indent \
	      --quoted --verbatim --upper \
	    | diff -u "${TMP_EXPT}" -
	then
		echo "sxcat test of configuration file overwrite failed"
		RET_VAL="1"
	fi

	rm "${TMP_SRC1}"
	rm "${TMP_SRC2}"
	rm "${TMP_DEST}"
	rm "${TMP_EXPT}"
else
	echo "${BIN_DIR}/sxcat not found"
fi

exit ${RET_VAL}
