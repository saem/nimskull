discard """
  targets: "!js"
  action: "compile"
"""

import terminal

proc test() {.raises:[IOError, ValueError].} =
  setBackgroundColor(stdout, bgRed)

test()
