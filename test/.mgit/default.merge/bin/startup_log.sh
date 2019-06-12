if [ -z "$PROMPT_COMMAND" ]; then
  (
    origin=${BASH_SOURCE[1]:-$HOME/.profile}
    log_dir=$XDG_RUNTIME_DIR
    if ! [ -w "$log_dir" ]; then
      log_dir=$HOME
    fi
    log_file=$log_dir/startup_log.txt

    echo ${origin#$HOME/}

    {
      date
      echo "$SHELL" "$origin"
      ps -f $(~/bin/ppids $$)
      env
      true
    } >> "$log_file"
  )
fi

# Local Variables:
# sh-basic-offset: 2
# End:
