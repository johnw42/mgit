IFS=

temp_files=()
trap 'rm -f ${temp_files[@]}' EXIT

script_file=$(mktemp)
temp_files+=($script_file)
output_file=$(mktemp)
temp_files+=($output_file)
expected_file=$(mktemp)
temp_files+=($expected_file)
strace_file=$(mktemp)
temp_files+=($strace_file)

try() {
  cat > $script_file
  chmod +x $script_file
  strace -o $strace_file -e trace=process $script_file  "$@" 2>&1 | sed "s:$script_file:script_file:g" >$output_file
  script_exit=$?s
}

expect() {
  cat > $expected_file
  if ! diff -u $expected_file $output_file; then
    echo "$BASH_SOURCE:$BASH_LINENO: unclean diff"
    cat $strace_file
    exit 1
  fi >&2
}

expect_success() {
  if [[ $script_exit != 0 ]]; then
    echo "$BASH_SOURCE:$BASH_LINENO: error in script:"
    echo ">>>"
    cat $script_file
    echo "<<<"
    exit 1
  fi >&2
}

expect_failure() {
  if [[ $script_exit == 0 ]]; then
    echo "$BASH_SOURCE:$BASH_LINENO: expected script to fail:"
    echo ">>>"
    cat $script_file
    echo "<<<"
    exit 1
  fi >&2
}

try <<EOF
#! $PWD/trampoline2
EOF
expect <<EOF
trampoline2: file ends prematurely: script_file
EOF

try <<EOF
#! $PWD/trampoline2
#
EOF
expect <<EOF
trampoline2: no command found on line 2 of script_file
EOF

try <<EOF
#! $PWD/trampoline2
#! echo \$0
EOF
expect <<EOF
script_file
EOF

try <<EOF
#! $PWD/trampoline2
##! echo \$0
EOF
expect <<EOF
script_file
EOF

try a 'b c' <<EOF
#! $PWD/trampoline2
#! escape -- args: \$0 \$*
EOF
expect <<EOF
args: script_file a 'b c'
EOF

try <<EOF
#! $PWD/trampoline2
#! bash \$0
echo "This is a bash script."
EOF
expect <<EOF
This is a bash script.
EOF


try a 'b c' <<EOF
#! $PWD/trampoline2
#! bash \$0 \$*
escape -- args: "\$@"
EOF
expect <<EOF
args: a 'b c'
EOF

# Local Variables:
# sh-basic-offset: 2
# End:
