#!/bin/sh

: ${BIN_DIR:=${1:-$(dirname $0)}}
: ${TMP_TEMPLATE:=/tmp/tmp.XXXXXX}

RET_VAL="0"

if ! test -x ${BIN_DIR}/hmac-md5 \
   -o -x ${BIN_DIR}/hmac-sha1 \
   -o -x ${BIN_DIR}/hmac-sha256; then
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

exit ${RET_VAL}
