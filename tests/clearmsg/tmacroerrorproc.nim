discard """
  errormsg: "Expected a node of kind nnkCharLit, got nnkSym"
  file: "tmacroerrorproc.nim"
  line: 13
"""
# issue #4915
import macros

macro mixer(n: typed): untyped =
  expectKind(n[0], nnkCharLit)

mixer:
  echo "owh"
