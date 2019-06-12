#! /bin/bash

# All this alias at the start of a function to trace it.
alias _trace_me="
echo \"\${FUNCNAME[0]}: enter\"
trap \"echo \${FUNCNAME[0]}: leave\" RETURN"

_init_utils_ext() {
  DEFINE_string bash_var_prefix

  local new_argv=()
  local arg dashdash_seen=false
  local flag flag_with_arg flag_arg flag_type
  for flag in "${!_flag_defaults[@]}"; do
    _set_flag "$flag" "${_flag_defaults[$flag]}"
  done
  for arg in "${argv[@]}"; do
    if $dashdash_seen || [[ $arg != -* ]]; then
      new_argv+=("$arg")
    elif [[ $arg = -- ]]; then
      dashdash_seen=true
    elif [[ $arg = --* ]]; then
      flag_with_arg=${arg#--}
      if [[ $flag_with_arg = *=* ]]; then
        flag=${flag_with_arg%%=*}
        flag_arg=${flag_with_arg#*=}
      else
        flag=$flag_with_arg
        flag_arg=true
      fi
      flag=${flag//-/_}
      flag_type=${_flag_types[$flag]}
      if [[ $flag_type ]]; then
        _set_flag "$flag" "$flag_arg"
      elif [[ ${_flag_types[${flag#no_}]} ]]; then
        _set_flag "${flag#no-}" false
      elif [[ ${_flag_types[${flag#no}]} ]]; then
        _set_flag "${flag#no}" false
      else
        user_error "unknown flag: --$flag"
      fi
    else
      user_error "can't parse argument: $arg"
    fi
  done

  argv=("${new_argv[@]}")
  argc=${#argv[*]}

  if [[ $FLAG_bash_var_prefix ]]; then
    exec 3>&1 1>/dev/null
  fi
}

_set_flag() {
  local name=$1 value=$2
  local type=${_flag_types[$name]}
  case $type in
    bool)
      case "$value" in
        t|y|1|true|yes|on) value=true ;;
        f|n|0|false|no|off|'') value= ;;
        *) user_error "invalid value \"$value\" for flag \"--$flag\""
      esac
      ;;
  esac
  unset "FLAG_$name"
  eval "FLAG_$name=\$value"
}

stack_trace() {
  # Print a stack trace, optionally skipping the first $1 frames.

  local i start_frame=${1-0}
  for i in "${!BASH_SOURCE[@]}"; do
    if (( i > start_frame )); then
      echo "${FUNCNAME[i]}:${BASH_SOURCE[i]}:${BASH_LINENO[i]}"
    fi
  done
}

warn() {
  # Print a warning message "$*" to stderr.

  local IFS=' '
  echo "$(basename "$0"): $*" >&2
}

# Check the number of arguments to a script.
_check_arg_count() {
  [[ $# = 1 ]] || die "invalid call to check_arg_count"
  local -i min=0 max=-1
  case $1 in
    *+) min=${1%+} ;;
    *..) min=${1%..} ;;
    *..*) min=${1%..*}; max=${1#*..} ;;
    *) min=$1; max=$1
  esac

  if (( argc < min )); then
    user_error "not enough arguments; expecting at least $min"
  fi
  if (( max != -1 && argc > max )); then
    user_error "too many arguments; expected at most $max"
  fi
}

user_error() {
  # Terminate the current script with an optional error message.

  if [[ $# = 0 ]]; then
    warn "fatal error"
  else
    warn "$@"
  fi

  # Exit out of a noninteractive shell.
  if [[ -z $PS1 ]]; then
    exit 1
  fi

  return 1
}

# Like user_error, but print a stack trace first.
die() {
  stack_trace 1
  user_error "$@"
}

declare -a _exit_trap_stack
trap _run_exit_traps EXIT

_run_exit_traps() {
  local cmd
  for cmd in "${_exit_trap_stack[@]}"; do
    eval "$cmd"
  done
}

# pop_array <array_name>
pop_array() {
  eval "local array_size=\${#$1[*]}"
  [[ $array_size -gt 0 ]] || die "empty stack: $1" || return
  eval "unset '$1[$array_size-1]'"
}

# trace_array <array_name>
trace_array() {
  # eval "local _keys=(\"\${!$1[@]}\")"
  # local _key _prefix=""
  # echo -n "$1=("
  # for _key in "${_keys[@]}"; do
  #   eval "echo -n \"$_prefix[\$_key]=\${$1[\$_key]}\""
  #   _prefix=" "
  # done
  # echo ")"
  local text=$(declare -p "$1")
  echo "${text#declare -a }"
}

# push_trap <cmd> EXIT
push_trap() {
  [[ $2 = EXIT ]] || die "invalid argument: $2" || return
  _exit_trap_stack+=("$1")
}

# pop_trap EXIT
pop_trap() {
  [[ $1 = EXIT ]] || die "invalid argument: $1" || return
  pop_array _exit_trap_stack
}

q() {
  local arg
  for arg; do
    arg=${arg//\'/\'\\\'\'}
    echo "'$arg'"
  done
}

get_vars() {
  local prefix=$1 cmd=$2
  shift 2
  eval "$("$cmd" --bash-var-prefix="$prefix" --bash-export-vars "$@")"
}

is_upper() {
  local -u upper=$1
  local -l lower=$1
  [[ $upper == $1 && $lower != $1 ]]
}

is_lower() {
  local -u upper=$1
  local -l lower=$1
  [[ $upper != $1 && $lower == $1 ]]
}

output_vars() {
  if [[ $FLAG_bash_var_prefix ]]; then
    local _var_in_var _out_var _val _decl
    for _var; do
      if [[ $_var = *=* ]]; then
        _in_var=${_var#*=}
        _out_var=${_var%=*}
      else
        _in_var=$_var
        _out_var=$_var
      fi
      _out_var=${FLAG_bash_var_prefix}_${_out_var}
      _decl=$(declare -p $_in_var)
      if [[ $FLAG_bash_export_vars ]] || is_upper "$FLAG_bash_var_prefix"; then
        _decl=${_decl/declare /export }
      fi
      _decl=${_decl/ $_in_var=/ $_out_var=}
      echo "$_decl" >&3
    done
  fi
}