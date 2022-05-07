#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements the dispatcher for the different parsers.

import
  strutils,
  ast/[
    llstream,
    ast,
    idents,
    lexer,
    parser,
    filters,
    filter_tmpl,
    renderer,
    lineinfos,
    reports
  ],
  front/[
    options,
    msgs,
  ],
  utils/[
    pathutils,
  ]

export Parser, parseAll, parseTopLevelStmt, closeParser

type
  FilterKind = enum
    filtNone = "none"
    filtTemplate = "stdtmpl"
    filtReplace = "replace"
    filtStrip = "strip"

proc utf8Bom(s: string): int =
  if s.len >= 3 and s[0] == '\xEF' and s[1] == '\xBB' and s[2] == '\xBF':
    3
  else:
    0

proc containsShebang(s: string, i: int): bool =
  if i+1 < s.len and s[i] == '#' and s[i+1] == '!':
    var j = i + 2
    while j < s.len and s[j] in Whitespace: inc(j)
    result = s[j] == '/'

proc parsePipe(filename: AbsoluteFile, inputStream: PLLStream; cache: IdentCache;
               config: ConfigRef): ParsedNode =
  result = newNode(pnkEmpty)
  var s = llStreamOpen(filename, fmRead)
  if s != nil:
    var line = newStringOfCap(80)
    discard llStreamReadLine(s, line)
    var i = utf8Bom(line)
    var linenumber = 1
    if containsShebang(line, i):
      discard llStreamReadLine(s, line)
      i = 0
      inc linenumber
    if i+1 < line.len and line[i] == '#' and line[i+1] == '?':
      when defined(nimpretty):
        # XXX this is a bit hacky, but oh well...
        config.quitOrRaise "can't nimpretty a source code filter: " & $filename
      else:
        inc(i, 2)
        while i < line.len and line[i] in Whitespace: inc(i)
        var p: Parser
        openParser(p, filename, llStreamOpen(substr(line, i)), cache, config)
        result = parseAll(p)
        closeParser(p)
    llStreamClose(s)

proc getFilter(ident: PIdent): FilterKind =
  for i in FilterKind:
    if cmpIgnoreStyle(ident.s, $i) == 0:
      return i

proc getCallee(conf: ConfigRef; n: ParsedNode): PIdent =
  if n.kind in pnkCallKinds and n[0].kind == pnkIdent:
    result = n[0].ident
  elif n.kind == pnkIdent:
    result = n.ident
  else:
    conf.localReport(n.info, ParserReport(kind: rparInvalidFilter, node: n))

proc applyFilter(p: var Parser, n: ParsedNode, filename: AbsoluteFile,
                 stdin: PLLStream): PLLStream =
  var f = getFilter(getCallee(p.lex.config, n))
  result = case f
           of filtNone:
             stdin
           of filtTemplate:
             filterTmpl(p.lex.config, stdin, filename, n)
           of filtStrip:
             filterStrip(p.lex.config, stdin, filename, n)
           of filtReplace:
             filterReplace(p.lex.config, stdin, filename, n)
  if f != filtNone:
    assert p.lex.config != nil
    if p.lex.config.hasHint(rlexSyntaxesCode):
      p.lex.config.localReport LexerReport(
        kind: rlexSyntaxesCode, msg: result.s)

proc evalPipe(p: var Parser, n: ParsedNode, filename: AbsoluteFile,
              start: PLLStream): PLLStream =
  assert p.lex.config != nil
  result = start
  if n.kind == pnkEmpty: return
  if n.kind == pnkInfix and n[0].kind == pnkIdent and n[0].ident.s == "|":
    for i in 1..2:
      if n[i].kind == pnkInfix:
        result = evalPipe(p, n[i], filename, result)
      else:
        result = applyFilter(p, n[i], filename, result)
  elif n.kind == pnkStmtList:
    result = evalPipe(p, n[0], filename, result)
  else:
    result = applyFilter(p, n, filename, result)

proc openParser*(p: var Parser, fileIdx: FileIndex, inputstream: PLLStream;
                  cache: IdentCache; config: ConfigRef) =
  assert config != nil
  let filename = toFullPathConsiderDirty(config, fileIdx)
  var pipe = parsePipe(filename, inputstream, cache, config)
  p.lex.config = config
  let s = if pipe != nil: evalPipe(p, pipe, filename, inputstream)
          else: inputstream
  parser.openParser(p, fileIdx, s, cache, config)

proc setupParser*(p: var Parser; fileIdx: FileIndex; cache: IdentCache;
                   config: ConfigRef): bool =
  let filename = toFullPathConsiderDirty(config, fileIdx)
  var f: File
  if not open(f, filename.string):
    config.localReport InternalReport(
      kind: rintCannotOpenFile, file: filename.string)
    return false
  openParser(p, fileIdx, llStreamOpen(f), cache, config)
  result = true

proc parseFile*(fileIdx: FileIndex; cache: IdentCache;
                config: ConfigRef): ParsedNode =
  var p: Parser
  if setupParser(p, fileIdx, cache, config):
    result = parseAll(p)
    closeParser(p)
