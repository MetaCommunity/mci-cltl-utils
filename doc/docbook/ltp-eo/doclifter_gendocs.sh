#!/usr/bin/env bash

set -e

## --

DOCLIFTER=/usr/local/bin/doclifter


## --

THIS=$(basename "$0")
HERE=$(dirname $(readlink -f "$0"))

msg() {
  echo "#-- ${THIS}: $@"
}

fail() {
  msg "$@" 1>&2
  exit
}

fail_doc_not_found(){
  fail "Not found: $@"
}

## --

ROFF=""

for DOCSPEC in "$@"; do
  SECTION=${DOCSPEC##*:}
  REFNAME=${DOCSPEC%:*}
  msg "Resolving ${REFNAME}(${SECTION}) from ${DOCSPEC}"
  SRCMAN=$(man -w "${SECTION}" "${REFNAME}" ||
               fail_doc_not_found "${DOCSPEC} => ${REFNAME}(${SECTION})")
  msg "Resolved to ${SRCMAN}"

  ## NB: This assumes that the manual page is gzipped - should dispach on filename
  #msg "Copying ${SRCMAN} to ${HERE}"
  msg "Unpacking ${SRCMAN} to ${FNAME}"

  FNAME=$(basename ${SRCMAN})
  FNAME=${FNAME%.gz}

  zcat "${SRCMAN}" > "${PWD}/${FNAME}"
  ROFF="${ROFF} ${FNAME}"
done

## FIXME - prefer mandoc -T html
## e.g mandoc -T html dlopen.3 
## b.c it does not add the strange "@GLUE@" token as in dlopen.3.xml

msg "Running doclifter"
${DOCLIFTER} -ww -x -h doclifter.hints ${ROFF}

