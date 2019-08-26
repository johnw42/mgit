IFS=
exit_code=0

assert_eq() {
  if [[ $1 != $2 ]]; then
    echo "$BASH_SOURCE:$BASH_LINENO: expected $1 == $2" 2>&1
    exit_code=1
  fi
}

assert_eq $(./escape -- '') "''"
assert_eq $(./escape -- a b) "a b"
assert_eq $(./escape -- 'a b') "'a b'"

exit $exit_code

# Local Variables:
# sh-basic-offset: 2
# End:
