# Get ready to panic.
PANIC_FILE=${XDG_RUNTIME_DIR:-$HOME}/bash_panic

# Convenient command for ending a panic.
end_panic() {
  if rm -f "$PANIC_FILE"; then
    source "${BASH_ENV:-$HOME/.bashrc}"
  fi
}

maybe_panic() {
  unset -f maybe_panic

  local countdown=10
  while [[ -f $PANIC_FILE ]]; do
    if (( --countdown == 0 )); then
      # Time to panic.  Avoid doing anything remotely questionable
      # during a panic.
      unset PROMPT_COMMAND
      PS1='end_panic? '
      echo "${BASH_SOURCE[1]}: panic: $PANIC_FILE"
      return 1
    fi
    # Wait for other shells to finish initializing before panicking.
    echo "Thinking about panicking..."
    sleep 0.$(( RANDOM % 100))
  done

  # Not panicking..
  touch "$PANIC_FILE"
  dont_panic() {
    rm -f "$PANIC_FILE"
    unset -f dont_panic
  }
}

maybe_panic

# Local Variables:
# sh-basic-offset: 2
# End:
