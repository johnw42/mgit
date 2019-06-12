#!/bin/bash

alias _trace_me="
echo \"\${FUNCNAME[0]}: enter\"
trap \"echo \${FUNCNAME[0]}: leave\" RETURN"

#alias _trace_me="echo \"\${FUNCNAME[0]}: enter\"; trap \"echo \${FUNCNAME[0]}: leave\" RETURN; set -e"

#trap 'echo err' ERR
#trap 'echo debug' DEBUG

trap 'echo exit' EXIT

alias err_trap='echo error; return 1'

_error_handler() {
  echo "Error occurred!"
}

xyzzy() {
  _trace_me
  xyzzy2
  echo "SHOULD NOT BE SEEN 1: $?"
  xyzzy2
}

xyzzy2() {
  _trace_me
  set -e
  trap 'set +e' ERR
  xyzzy3
  echo "SHOULD NOT BE SEEN 2: $?"
  xyzzy3
}

xyzzy3() {
  _trace_me
  kill -TERM $BASHPID
  echo "SHOULD NOT BE SEEN 3: $?"
}

err_code() {
  return $1
}