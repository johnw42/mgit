#! /usr/bin/gawk -f

# This program takes the input from diff and produces a mapping
# from the numbers in the old file into the numbers in the new file.
#
# The output has one space-separated pair per line. The first number is
# from the old file and the second number is from the new file. The first
# line of the output describes the relation as "1-1", "1-n", "n-1" or "n-n".
# Identities are not included in the output but the will affect how the map is
# described. For instance, this relation: { (3,4), (4,4) } will produce this
# output:
#
# n-1
# 3 4

BEGIN { from = "1"; to = "1" }

function procbufs() {
  # Process the buffers.
  if (oldbuf_size > 0 && oldbuf_size == newbuf_size) {
    for (i in oldbuf) {
      oldstring = oldbuf[i]
      newstring = newbuf[i]
      if (gsub(/[0-9]+/, "0", oldstring) == \
            gsub(/[0-9]+/, "0", newstring) && \
          oldstring == newstring)
      {
        # There is a mapping between the numbers in the old string
        # and the new string.
        split(oldbuf[i], oldsplit, /[^0-9]+/)
        split(newbuf[i], newsplit, /[^0-9]+/)
        for (j in oldsplit) {
          old = oldsplit[j]
          new = newsplit[j]
          if (!(old in old_new_map)) {
            old_new_map[old] = new
          }  else if (old_new_map[old] != new) {
            old_new_map[old] = old_new_map[old] "," new
            to = "n"
          }
          if (!(new in new_old_map)) {
            new_old_map[new] = old
          }  else if (new_old_map[new] != old) {
            new_old_map[new] = new_old_map[new] "," old
            from = "n"
          }
        }
      }
    }
  }
}

/^[0-9]/ {
  procbufs()
  
  # Clear the buffers.
  oldbuf_size = 0
  newbuf_size = 0
  delete oldbuf
  delete newbuf
}

# Save lines into the buffers.
/^< / { oldbuf[oldbuf_size++] = substr($0, 3) }
/^> / { newbuf[newbuf_size++] = substr($0, 3) }

END {
  procbufs()
  
  # Print out the mapping.
  print from "-" to
  for (i in old_new_map) {
    split(old_new_map[i], rhs, ",")
    delete rhs_unique
    for (j in rhs)
      rhs_unique[rhs[j]] = 1
    for (j in rhs_unique)
      if (i != j)
        print i " " j
  }
}

# Options for vi: ts=2 sw=2 sts=2 nowrap expandtab ft=awk
