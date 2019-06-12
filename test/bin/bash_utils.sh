#! /bin/bash

IFS=
set -e
shopt -s expand_aliases
alias init_bash_utils='_init_utils "$@"; set -- "${argv[@]}"; _check_arg_count'

_init_utils() {
  shopt -u expand_aliases
  argv=("$@")
  source bash_utils_ext.sh
  _init_utils_ext
}

declare -A _flag_types _flag_defaults _output_vars

_declare_flag() {
  local type=$1 name=$2 default=$3 desc=$4
  _flag_types[$name]=$type
  _flag_defaults[$name]=$default
}

DEFINE_string() {
  _declare_flag string "$@"
}

DEFINE_bool() {
  _declare_flag bool "$@"
}
