source ~/bin/bashrc_panic.sh || return

source ~/.bash_profile

# Load __git_ps1 for later.
source /etc/bash_completion.d/git-prompt

PROMPT_DIRTRIM=3

########################################################################
##  ==> .bashrc.d/android.sh <==
##

export ANDROID_SDK_HOME=/google/data/ro/teams/mobile_eng_prod/android_sdk_linux
export PATH=${PATH}:${ANDROID_SDK_HOME}/tools:${ANDROID_SDK_HOME}/platform-tools

# Use adb.turbo when possible, otherwise fall back to standard adb.
function adb() {
  EMU_DEPS=/google/data/ro/teams/mobile_eng_prod/emu/live/google3/
  ANDROID_SDK=${EMU_DEPS}/third_party/java/android/android_sdk_linux/
  EMU_SUPPORT=${EMU_DEPS}/tools/android/emulator/support/
  ANDROID_ADB=${ANDROID_SDK}/platform-tools/adb
  ANDROID_ADB=${ANDROID_ADB} $EMU_SUPPORT/adb.turbo "$@"
}

########################################################################
##  ==> .bashrc.d/cd5.sh <==
##

# Switcht to a git5 google3 directory.
cd5() {
  if [[ $1 = /* ]]; then
    cd "$1"
  else
    cd ~/git5/"$1"/google3
  fi
}

_cd5_complete() {
  local prefix=$HOME/git5/ suffix=/google3 i
  COMPREPLY=($(shopt -s nullglob; echo $prefix"$2"*$suffix))
  COMPREPLY=("${COMPREPLY[@]#$prefix}")
  COMPREPLY=("${COMPREPLY[@]%$suffix}")
}

complete -F _cd5_complete cd5

########################################################################
##  ==> .bashrc.d/completion.sh <==
##

: ${sourced_bash_completion:=false}
if ! ${sourced_bash_completion}; then
  sourced_bash_completion=true
  if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

########################################################################
##  ==> .bashrc.d/debug_trace.sh <==
##

# shopt -s extdebug
# rm -f "$XDG_RUNTIME_DIR/bash_trace.txt"
# exec 3>"$XDG_RUNTIME_DIR/bash_trace.txt"
# BASH_XTRACEFD=3
# set -x
# trap 'date -Ins >&3; : ${BASH_SOURCE[*]}' DEBUG RETURN

# Local Variables:
# sh-basic-offset: 2
# End:

########################################################################
##  ==> .bashrc.d/diff_alias.sh <==
##

alias diff=meld

# Local Variables:
# sh-basic-offset: 2
# End:

########################################################################
##  ==> .bashrc.d/google.sh <==
##

# -*- mode: shell-script; sh-basic-offset: 2 -*-

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#vcsh ghome -c color.status=always status --short | sed -e 's/^/ghome: /'

# if $starting_new_shell; then
#   # Detect files shared between repositories.
#   (
#     IFS=
#     dir=$(mktemp -d)
#     cd $dir
#     trap 'cd; rm -rf $dir' EXIT
#     for repo in home ghome; do
#       vcsh $repo ls-files | sort >$repo
#     done
#     comm -12 home ghome >common
#     if ! cmp -s common /dev/null; then
#       echo "$BASH_SOURCE: Double-tracked files!"
#       echo -n "vcsh <repo> rm --cached "
#       tr '\n' ' ' <common
#       echo
#     fi
#   )
# fi


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if $starting_new_shell && $color_terminal && [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    command alias grep='grep --color=auto'
    command alias fgrep='fgrep --color=auto'
    command alias egrep='egrep --color=auto'
fi

# some more ls aliases
command alias ll='ls -alF'
command alias la='ls -A'
command alias l='ls -CF'
command alias rm='rm -i'
command alias mv='mv -i'
command alias cp='cp -i'
command alias sl=ls
command alias sls=ls
command alias ..='cd ..'
command alias ...='cd ../..'
command alias unfunction='unset -f'
command alias py=python

# cd up to the nearest google3 directory
g3() {
  _require_g3 && cd "$G3_DIR" && pwd
}

man() {
  case "$1" in
    ack) shift; set -- ack-grep "$@" ;;
  esac
  command man "$@"
  if [[ $? = 16 && $# = 1 ]] && type "$1" >/dev/null 2>&1; then
    prog=$1
    eval '"$prog" --help' | less
    return ${PIPESTATUS[0]}
  fi
}

_bbin_complete() {
  COMPREPLY=()
  [[ -d $G3_DIR ]] || return $?
  local cur=$(_get_cword) name
  if [[ $COMP_CWORD = 1 ]]; then
    local subdir=
    if [[ $cur = */* ]]; then
      subdir=${cur%/*}
    fi
    local IFS='
'
    COMPREPLY=($(find "$G3_DIR/blaze-bin$G3_SUBDIR/$subdir" -maxdepth 1 -type f -executable -name "${cur##*/}*" -printf '%f\n'))
    if [[ -n $subdir ]]; then
      local i
      for (( i=0; i < ${#COMPREPLY[@]}; i++ )); do
        COMPREPLY[i]=$subdir/${COMPREPLY[i]}
      done
    fi
  else
    _filedir
  fi
}

# Completion of blaze abbreviations.  Taken from
# /etc/bash_completion.d/blaze
complete -o nospace -F _blaze::complete_build_target_wrapper bb br bbr
complete -o plusdirs -F _bbin_complete bbin

shopt -u failglob
shopt -s extglob globstar
shopt -s histappend histreedit histverify


# Switcht to a git5 google3 directory.
cd5() {
  if [[ $1 = /* ]]; then
    cd "$1"
  else
    cd ~/git5/"$1"/google3
  fi
}

_cd5_complete() {
  local prefix=$HOME/git5/ suffix=/google3 i
  COMPREPLY=($(shopt -s nullglob; echo $prefix"$2"*$suffix))
  COMPREPLY=("${COMPREPLY[@]#$prefix}")
  COMPREPLY=("${COMPREPLY[@]%$suffix}")
}

complete -F _cd5_complete cd5


# Echo the name of directory with abbreviations.
echodir() {
  if [[ $1 == $HOME/* ]]; then
    echo "~/${1#$HOME/}"
  else
    echo "$1"
  fi
}

declare -a CDHIST
declare -i CDHISTPOS

# cd history
#
# Running 'cdhist' with no arguments prints the history.
# Running with a numerical argument changes to the specified place in the history.
# Running with 'clear' as the argument resets the history.
cdhist() {
  local i prefix

  if [[ $1 == update ]]; then
    if [[ ${CDHIST[CDHISTPOS]} != $PWD ]]; then
      if [[ ${CDHIST[CDHISTPOS]} == $OLDPWD ]]; then
        for CDHISTPOS in ${!CDHIST[*]}; do
          [[ ${CDHIST[$CDHISTPOS]} == $PWD ]] && break
        done
      fi
      if [[ ${CDHIST[CDHISTPOS]} != $PWD ]]; then
        CDHIST=("$PWD" "${CDHIST[@]:$CDHISTPOS:10}" "${CDHIST[@]:0:$CDHISTPOS}")
        CDHISTPOS=0
      fi
    fi
  elif [[ $1 == clear ]]; then
    CDHISTPOS=0
    CDHIST=("$PWD")
  elif [[ $# == 0 ]]; then
    for i in ${!CDHIST[*]}; do
      prefix=' '
      if [[ $i == $CDHISTPOS ]]; then
        prefix='+'
      elif [[ ${CDHIST[i]} == $OLDPWD ]]; then
        prefix='-'
      fi
      echo "$prefix $i $(echodir "${CDHIST[i]}" G3_DIR)"
    done
  else
    CDHISTPOS=$1
    cd "${CDHIST[CDHISTPOS]}"
  fi
}

color_terminal=true
if [[ $EMACS = t ]]; then
  color_terminal=false
fi

alias pgrep=pgrep-wrapper


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

#cdhist update

alias ls='ls --color=auto'

# TODO(jrw): Delete this?
# _ls_helper() {
#   if [[ -t 1 ]]; then
#     # If stdout is a terminal...
#     # WARNING: enabling this can cause multi-second delays due to NFS latency
#     run_with_pty ls --color "$@" | cat
#   else
#     command ls "$@"
#   fi
# }
# alias ls=_ls_helper

if $starting_new_shell; then
  if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
  fi
fi

export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox

export ANDROID_SERIAL=172.31.27.164:5555
export DEFAULT_JVM_DEBUG_SUSPEND=n
export PYTHONSTARTUP=~/.pythonrc.py
export PYTHONPATH=/home/build:~/bin/python
export P4DIFF=true
export LESS=
LESS="$LESS --search-skip-screen"
LESS="$LESS --ignore-case"        # -+i
#LESS="$LESS --status-column"      # -+J
#LESS="$LESS --LINE-NUMBERS"       # -+N
LESS="$LESS --RAW-CONTROL-CHARS"  # -+R
LESS="$LESS --chop-long-lines"    # -+S

# echopath() {
#   local IFS=: entry
#   for entry in $1; do
#     echo "$entry"
#   done
# }


if $starting_new_shell; then
  _tmp() {
    local blaze_bin subdir
    for blaze_bin in blaze-bin .; do
      for subdir in . sender receiver; do
        PATH+=:$blaze_bin/java/com/google/chrome/cloudview/script/$subdir
      done
    done
  }
  _tmp
fi

unset -f _tmp

########################################################################
##  ==> .bashrc.d/gyp_defines.sh <==
##

export GYP_DEFINES="run_jscompile=0"

########################################################################
##  ==> .bashrc.d/history.sh <==
##

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=9000

# Local Variables:
# sh-basic-offset: 2
# End:

########################################################################
##  ==> .bashrc.d/legacy.sh <==
##

# -*- mode: shell-script; sh-basic-offset: 2 -*-

# Load crazy experimental stuff.
TODAY=$(date -I)
[[ -f ~/bashrc.$TODAY ]] && . ~/bashrc.$TODAY


# maybe_append() {
#   local _var=$1 _sep=$2
#   eval "local _val=\$$_var"
#   shift 2
#   for _arg in "$@"; do
#     if [[ $_sep$_val$_sep != *${_sep}${_arg}${_sep}* ]]; then
#       if [[ $_val == '' ]]; then
# 	# Special case of a totally empty variable.
# 	_val=$_arg
#       elif [[ $_val == $_sep ]]; then
# 	# Special case of a variable containing just a separator.
# 	_val+=$_arg
#       else
# 	# Typical case.
# 	_val+=$_sep$_arg
#       fi
#     fi
#   done
#   eval "$_var=\$_val"
# }


# # Random stuff.
# maybe_append PATH : ~/.local/bin ~/Downloads/go/bin
# maybe_append PROMPT_COMMAND ';' 'nag ps1'

########################################################################
##  ==> .bashrc.d/misc.sh <==
##

# -*- mode: shell-script; sh-basic-offset: 2 -*-

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if $starting_new_shell && $color_terminal && [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    command alias grep='grep --color=auto'
    command alias fgrep='fgrep --color=auto'
    command alias egrep='egrep --color=auto'
fi

# some more ls aliases
command alias ll='ls -alF'
command alias la='ls -A'
command alias l='ls -CF'
command alias rm='rm -i'
command alias mv='mv -i'
command alias cp='cp -i'
command alias sl=ls
command alias sls=ls
command alias ..='cd ..'
command alias ...='cd ../..'
command alias unfunction='unset -f'
command alias py=python

man() {
  case "$1" in
    ack) shift; set -- ack-grep "$@" ;;
  esac
  command man "$@"
  if [[ $? = 16 && $# = 1 ]] && type "$1" >/dev/null 2>&1; then
    prog=$1
    eval '"$prog" --help' | less
    return ${PIPESTATUS[0]}
  fi
}

shopt -u failglob
shopt -s extglob globstar
shopt -s histappend histreedit histverify


# Echo the name of directory with abbreviations.
echodir() {
  if [[ $1 == $HOME/* ]]; then
    echo "~/${1#$HOME/}"
  else
    echo "$1"
  fi
}

declare -a CDHIST
declare -i CDHISTPOS

# cd history
#
# Running 'cdhist' with no arguments prints the history.
# Running with a numerical argument changes to the specified place in the history.
# Running with 'clear' as the argument resets the history.
cdhist() {
  local i prefix

  if [[ $1 == update ]]; then
    if [[ ${CDHIST[CDHISTPOS]} != $PWD ]]; then
      if [[ ${CDHIST[CDHISTPOS]} == $OLDPWD ]]; then
        for CDHISTPOS in ${!CDHIST[*]}; do
          [[ ${CDHIST[$CDHISTPOS]} == $PWD ]] && break
        done
      fi
      if [[ ${CDHIST[CDHISTPOS]} != $PWD ]]; then
        CDHIST=("$PWD" "${CDHIST[@]:$CDHISTPOS:10}" "${CDHIST[@]:0:$CDHISTPOS}")
        CDHISTPOS=0
      fi
    fi
  elif [[ $1 == clear ]]; then
    CDHISTPOS=0
    CDHIST=("$PWD")
  elif [[ $# == 0 ]]; then
    for i in ${!CDHIST[*]}; do
      prefix=' '
      if [[ $i == $CDHISTPOS ]]; then
        prefix='+'
      elif [[ ${CDHIST[i]} == $OLDPWD ]]; then
        prefix='-'
      fi
      echo "$prefix $i $(echodir "${CDHIST[i]}" G3_DIR)"
    done
  else
    CDHISTPOS=$1
    cd "${CDHIST[CDHISTPOS]}"
  fi
}

color_terminal=true
if [[ $EMACS = t ]]; then
  color_terminal=false
fi

alias pgrep=pgrep-wrapper


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

#cdhist update

alias ls='ls --color=auto'

# TODO(jrw): Delete this?
# _ls_helper() {
#   if [[ -t 1 ]]; then
#     # If stdout is a terminal...
#     # WARNING: enabling this can cause multi-second delays due to NFS latency
#     run_with_pty ls --color "$@" | cat
#   else
#     command ls "$@"
#   fi
# }
# alias ls=_ls_helper

export PYTHONSTARTUP=~/.pythonrc.py
export PYTHONPATH=/home/build:~/bin/python
export LESS=
LESS="$LESS --search-skip-screen"
LESS="$LESS --ignore-case"        # -+i
#LESS="$LESS --status-column"      # -+J
#LESS="$LESS --LINE-NUMBERS"       # -+N
LESS="$LESS --RAW-CONTROL-CHARS"  # -+R
LESS="$LESS --chop-long-lines"    # -+S

# echopath() {
#   local IFS=: entry
#   for entry in $1; do
#     echo "$entry"
#   done
# }

unset -f _tmp

########################################################################
##  ==> .bashrc.d/prodaccess.sh <==
##

alias blaze="_ensure_prodaccess blaze"
alias rabbit="_ensure_prodaccess rabbit"

########################################################################
##  ==> .bashrc.d/pstree.sh <==
##

pstree() {
  if [[ -t 1 ]]; then
    command pstree --unicode "$@" | less
  else
    command pstree "$@"
  fi
}

########################################################################
##  ==> .bashrc.d/set_window_title.sh <==
##

# Update the terminal window title.
if [[ $NXSESSION ]]; then
  _wintitle NX Session
else
  (
    title=
    if [[ -n $SSH_CONNECTION ]]; then
      title="$(hostname -s): "
    fi
    title+=$PWD
    _wintitle "$title"
  )
fi

# Local Variables:
# sh-basic-offset: 2
# End:

########################################################################
##  ==> .bashrc.d/sync_history.sh <==
##

# Synchronize shell history between instances.
# http://unix.stackexchange.com/a/48116
history -a
HISTFILESIZE=$HISTSIZE
history -c
history -r

########################################################################
##  ==> .bashrc.d/ZZ_00.sh <==
##

# -*- mode: shell-script; sh-basic-offset: 2 -*-
# This file is executed last.

########################################################################
##  ==> .bashrc.d/zz_depot_tools.sh <==
##

PATH=$HOME/depot_tools:$PATH

########################################################################
##  ==> .bashrc.d/ZZ_local_prompt.sh <==
##

#1e89500b-0672-4c2c-86b7-978687c472e3
[[ -f .local_bash_prompt ]] && source .local_bash_prompt

bashrc() {
  source ~/.bashrc
}

# Reload this file on every command prompt.  This file should execute
# quickly enough that there is no noticeable delay.
prompt_command() {
  local last_exit_status=$?
  if $starting_new_shell; then
    per_tty
  fi
  per_prompt
  starting_new_shell=false

  # Termianl colors:
  # 0 - black
  # 1 - red
  # 2 - green
  # 3 - yellow
  # 4 - blue
  # 5 - magenta
  # 6 - cyan
  # 7 - white
  local ps1_reset="\\[$(tput sgr0)\\]"
  local ps1_plain="$ps1_reset\\[$(tput setaf 6)\\]"

  PS1=$ps1_plain
  if [[ -n $SSH_CONNECTION ]]; then
    PS1+="\\u@\\h:"
  fi
  if git rev-parse --git-dir &>/dev/null; then
    # Prompt for git.
    local git_toplevel=$(git rev-parse --show-toplevel)
    PS1+=${git_toplevel##*/}
    local GIT_PS1_SHOWDIRTYSTATE=
    local GIT_PS1_SHOWSTASHSTATE=
    local GIT_PS1_SHOWUNTRACKEDFILES=
    local GIT_PS1_SHOWUPSTREAM=auto
    local GIT_PS1_SHOWCOLORHINTS=t
    __git_ps1 "$PS1$ps1_reset" "$ps1_plain"
    PS1+=${PWD#$git_toplevel}
  elif [[ $PWD = /google/src/cloud/jrw/* ]]; then
    # Prompt for citc.
    PS1+="(citc)${PWD#/google/src/cloud/jrw/}"
  else
    # Default prompt.
    PS1+="\\w"
  fi
  PS1+="$ps1_plain\\\$"
  if [[ $last_exit_status != 0 ]]; then
    PS1+="\\[$(tput bold; tput setaf 5)\\][$last_exit_status]"
  fi
  PS1+="$ps1_reset "
}

PROMPT_COMMAND=prompt_command
prompt_command

dont_panic; return

# Local Variables:
# sh-basic-offset: 2
# End:
