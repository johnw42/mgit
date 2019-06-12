#! /bin/bash

set +x



{ echo line 1; echo line 2; echo line 3; } | while read line; do
  echo "read: $line"
done
