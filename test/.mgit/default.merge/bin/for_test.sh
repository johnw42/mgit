#! /bin/bash

echo_word() {
  echo "[$word]"
}

all_tests=
q() {
  all_tests+=" $1"
}

q test1
test1() {
  local words="a b c"
  for word in $words; do
    echo_word
  done
}

q test2
test2() {
  local lines="a
b
c"
  for word in $lines; do
    echo_word
  done
}

q test3
test3() {
  seq 3
  for word in $(seq 3); do
    echo_word
  done
}

q test1_q
test1_q() {
  local words="a b c"
  for word in "$words"; do
    echo_word
  done
}

q test2_q
test2_q() {
  local lines="a
b
c"
  for word in "$lines"; do
    echo_word
  done
}


q test3_q
test3_q() {
  for word in "$(seq 3)"; do
    echo_word
  done
}

q test1_no_ifs
test1_no_ifs() {
  IFS=
  local words="a b c"
  for word in $words; do
    echo_word
  done
}

q test2_no_ifs
test2_no_ifs() {
  IFS=
  local lines="a
b
c"
  for word in $lines; do
    echo_word
  done
}

q test3_no_ifs
test3_no_ifs() {
  IFS=
  for word in $(seq 3); do
    echo_word
  done
}


q test1_special_ifs
test1_special_ifs() {
  IFS=:
  local words="a b c"
  for word in $words; do
    echo_word
  done
}

q test2_special_ifs
test2_special_ifs() {
  IFS=:
  local lines="a
b
c"
  for word in $lines; do
    echo_word
  done
}

q test3_special_ifs
test3_special_ifs() {
  IFS=:
  for word in $(seq 3); do
    echo_word
  done
}


q test4_special_ifs
test4_special_ifs() {
  IFS=:
  local array=(a b c)
  for word in ${array[*]}; do
    echo_word
  done
}


q test5_special_ifs
test5_special_ifs() {
  IFS=:
  local array=(a b c)
  for word in ${array[@]}; do
    echo_word
  done
}

q test6_special_ifs
test6_special_ifs() {
  IFS=:
  local array=(a b c)
  for word in "${array[*]}"; do
    echo_word
  done
}


q test7_special_ifs
test7_special_ifs() {
  IFS=:
  local array=(a b c)
  for word in "${array[@]}"; do
    echo_word
  done
}

q change_ifs
change_ifs() {
  local words="a b c"
  IFS=
  for word in $words; do
    IFS=' '
    echo_word
  done
  for word in $words; do
    IFS=' '
    echo_word
  done
}

all_tests() {
  for test_name in $all_tests; do
    echo ''
    echo $test_name
    ($test_name) 2>&1
  done
}

main() {
  all_tests >$0.output
  all_tests | diff -c $0.output -
}

main; exit
