#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements Nim's simple filters and helpers for filters.

import
  ast/[
    llstream,
    ast,
    renderer,
    reports
  ],
  std/[
    strutils,
  ],
  utils/[
    pathutils,
  ],
  front/[
    msgs,
    options,
  ]


proc invalidPragma(conf: ConfigRef; n: ParsedNode) =
  conf.localReport(n.info, reportAst(rsemNodeNotAllowed, n))

proc getArg(conf: ConfigRef; n: ParsedNode, name: string, pos: int): ParsedNode =
  result = nil
  if n.kind in {pnkEmpty..pnkNilLit}: return
  for i in 1..<n.len:
    if n[i].kind == pnkExprEqExpr:
      if n[i][0].kind != pnkIdent: invalidPragma(conf, n)
      if cmpIgnoreStyle(n[i][0].ident.s, name) == 0:
        return n[i][1]
    elif i == pos:
      return n[i]

proc charArg*(conf: ConfigRef; n: ParsedNode, name: string, pos: int, default: char): char =
  var x = getArg(conf, n, name, pos)
  if x == nil: result = default
  elif x.kind == pnkCharLit: result = chr(int(x.intVal))
  else: invalidPragma(conf, n)

proc strArg*(conf: ConfigRef; n: ParsedNode, name: string, pos: int, default: string): string =
  var x = getArg(conf, n, name, pos)
  if x == nil: result = default
  elif x.kind in {pnkStrLit..pnkTripleStrLit}: result = x.strVal
  else: invalidPragma(conf, n)

proc boolArg*(conf: ConfigRef; n: ParsedNode, name: string, pos: int, default: bool): bool =
  var x = getArg(conf, n, name, pos)
  if x == nil: result = default
  elif x.kind == pnkIdent and cmpIgnoreStyle(x.ident.s, "true") == 0: result = true
  elif x.kind == pnkIdent and cmpIgnoreStyle(x.ident.s, "false") == 0: result = false
  else: invalidPragma(conf, n)

proc filterStrip*(conf: ConfigRef; stdin: PLLStream, filename: AbsoluteFile, call: ParsedNode): PLLStream =
  var pattern = strArg(conf, call, "startswith", 1, "")
  var leading = boolArg(conf, call, "leading", 2, true)
  var trailing = boolArg(conf, call, "trailing", 3, true)
  result = llStreamOpen("")
  var line = newStringOfCap(80)
  while llStreamReadLine(stdin, line):
    var stripped = strip(line, leading, trailing)
    if pattern.len == 0 or startsWith(stripped, pattern):
      llStreamWriteln(result, stripped)
    else:
      llStreamWriteln(result, line)
  llStreamClose(stdin)

proc filterReplace*(conf: ConfigRef; stdin: PLLStream, filename: AbsoluteFile, call: ParsedNode): PLLStream =
  var sub = strArg(conf, call, "sub", 1, "")
  if sub.len == 0: invalidPragma(conf, call)
  var by = strArg(conf, call, "by", 2, "")
  result = llStreamOpen("")
  var line = newStringOfCap(80)
  while llStreamReadLine(stdin, line):
    llStreamWriteln(result, replace(line, sub, by))
  llStreamClose(stdin)
