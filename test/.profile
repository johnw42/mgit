# Profile script run by all shells (including bash, because it is
# sourced from .bash_profile).

: ${starting_new_shell:=true}

if ! [ -w "$XDG_RUNTIME_DIR" ]; then
  export XDG_RUNTIME_DIR=$(mktemp -d)
fi

. ~/bin/startup_log.sh

export AT_GOOGLE=false
if [ -d /google ]; then
  AT_GOOGLE=true
fi

export PATH=${PROFILE_DEFAULT_PATH:=$PATH}
export INFOPATH=${PROFILE_DEFAULT_INFOPATH:=$INFOPATH}:
export PKG_CONFIG_PATH=${PROFILE_DEFAULT_PKG_CONFIG_PATH:=$PKG_CONFIG_PATH}
export PYTHONPATH=${PROFILE_DEFAULT_PYTHONPATH:=$PYTHONPATH}:
export MANPATH=${PROFILE_DEFAULT_MANPATH:=$MANPATH}:

export PREFIX=$HOME/.local
unset IFS

if $AT_GOOGLE; then
  PATH=$HOME/goma:$HOME/depot_tools:$PATH
fi

PATH=$HOME/pbin:$HOME/bin:$HOME/.cabal/bin:$PREFIX/bin:${PROFILE_DEFAULT_PATH:=$PATH}
MANPATH=$PREFIX/man:$MANPATH
INFOPATH=$HOME/src/emacs-24.2/info:$PREFIX/info:$INFOPATH
PKG_CONFIG_PATH=$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH
PYTHONPATH=$HOME/bin/python:$PYTHONPATH

unset ALTERNATE_EDITOR
export EDITOR='emacs --no-desktop'
export VISUAL=$EDITOR
export SUDO_EDITOR="emacs --no-desktop -u $USER"

if $AT_GOOGLE && [ -z "$CHROME_REMOTE_DESKTOP_SESSION" ]; then
  export XDG_CONFIG_HOME=$HOME/.unity/config
  export XDG_DATA_HOME=$HOME/.unity/share
  export XDG_CACHE_HOME=$HOME/.unity/cache
else
  export XDG_CONFIG_HOME=$HOME/config
  export XDG_DATA_HOME=$PREFIX/share
  export XDG_CACHE_HOME=$HOME/cache
fi

per_login

# Local Variables:
# sh-basic-offset: 2
# EAnd:
