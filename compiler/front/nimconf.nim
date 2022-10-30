#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module handles the reading of the config file.

import
  std/[
    os,
    strutils,
    strtabs,
  ],
  compiler/front/[
    commands,
    options,
    scriptconfig
  ],
  compiler/ast/[
    lexer,
    idents,
    wordrecg,
    llstream,
    lineinfos,
    ast
  ],
  compiler/utils/[
    pathutils,
  ]

type
  # TODO:
  # - DONE: finish mapping the diags enum below
  # - HERE: finish converting this file
  # - then figure out how to make the handler pluggable
  ConfigEventKind* = enum
    ## events/errors arising from parsing and processing a compiler config file

    # users errors begin
    # expected token
    cekParseExpectedX        ## expected some token
    cekParseExpectedCloseX   ## expected closing ')', ']', etc
    cekParseExpectedIdent    ## expected an identifier

    # invalid input
    cekInvalidDirective
    # user errors end

    # user output start
    cekWriteConfig           ## write out the config
    # user output end

    # debug start
    cekDebugTrace            ## config debug trace
    cekDebugReadStart        ## start config file read
    cekDebugReadStop         ## stop config file read
    # debug end

    # progress begin
    cekProgressConfStart
    # progress end

  ConfigFileEvent* = object
    case kind*: ConfigEventKind:
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
         cekInvalidDirective, cekWriteConfig, cekDebugTrace:
        location*: TLineInfo         ## diagnostic location
      of cekDebugReadStart, cekDebugReadStop, cekProgressConfStart:
        discard
    instLoc*: InstantiationInfo ## instantiation in lexer's source
    msg*: string

# ---------------- configuration file parser -----------------------------
# we use Nim's lexer here to save space and work

# xxx: importing `reports`, the whole need to bridge via `handleDiagReport`
#      it's all terrible. this needs to be further broken up so the lexer just
#      emits diagnostics and is configured as to how it should handle them as
#      they arise wrt to aborting, etc
from compiler/ast/report_enums import ReportKind, ReportCategory
from compiler/ast/reports import Report, DebugReport, ExternalReport,
                                 InternalReport, LexerReport, ParserReport,
                                 toReportLineInfo
from compiler/front/msgs import handleReport
import std/options as std_options

proc handleConfigEvent(
    conf: ConfigRef, 
    evt: ConfigFileEvent,
    reportFrom: InstantiationInfo,
    eh: TErrorHandling = doNothing
  ) {.inline.} =
  # REFACTOR: this is a temporary bridge into existing reporting

  let kind =
    case evt.kind
    of cekParseExpectedX: rlexExpectedToken
    of cekParseExpectedCloseX: rlexExpectedToken
    # xxx: rlexExpectedToken is not a "lexer" error, but attempts at code reuse
    #      perpetuated it -- fix after reporting is untangled.
    of cekParseExpectedIdent: rparIdentExpected
    of cekInvalidDirective: rlexCfgInvalidDirective
    of cekWriteConfig: rintNimconfWrite
    of cekDebugTrace: rdbgCfgTrace
    of cekDebugReadStart: rdbgStartingConfRead
    of cekDebugReadStop: rdbgFinishedConfRead
    of cekProgressConfStart: rextConf

  var rep =
    case kind
    of rlexExpectedToken, rlexCfgInvalidDirective:
      Report(
        category: repLexer,
        lexReport: LexerReport(
          location: std_options.some evt.location,
          reportInst: evt.instLoc.toReportLineInfo,
          msg: evt.msg,
          kind: kind))
    of rparIdentExpected:
      Report(
        category: repParser,
        parserReport: ParserReport(
          location: std_options.some evt.location,
          reportInst: evt.instLoc.toReportLineInfo,
          msg: evt.msg,
          kind: kind))
    of rintNimconfWrite:
      Report(
        category: repInternal,
        internalReport: InternalReport(
          location: std_options.some evt.location,
          reportInst: evt.instLoc.toReportLineInfo,
          msg: evt.msg,
          kind: kind))
    of rdbgCfgTrace:
      Report(
        category: repDebug,
        debugReport: DebugReport(
          location: std_options.some evt.location,
          reportInst: evt.instLoc.toReportLineInfo,
          kind: kind,
          str: evt.msg))
    of rdbgStartingConfRead, rdbgFinishedConfRead:
      Report(
        category: repDebug,
        debugReport: DebugReport(
          reportInst: evt.instLoc.toReportLineInfo,
          kind: kind,
          filename: evt.msg))
    of rextConf:
      Report(
        category: repExternal,
        externalReport: ExternalReport(
          reportInst: evt.instLoc.toReportLineInfo,
          kind: kind,
          msg: evt.msg))
    else:
      doAssert false, "totally borked"
      Report() # assert ensure we don't get here
  
  handleReport(conf, rep, reportFrom, eh)

template handleError(L: Lexer, ev: ConfigEventKind, errMsg: string): untyped =
  {.line.}:
    let e = ConfigFileEvent(kind: ev,
                            location: L.getLineInfo,
                            instLoc: instLoc(),
                            msg: errMsg)
    L.config.handleConfigEvent(e, instLoc())

template handleExpectedX(L: Lexer, missing: string): untyped =
  {.line.}:
    let e = ConfigFileEvent(kind: cekParseExpectedX, 
                            location: L.getLineInfo, 
                            instLoc: instLoc(), 
                            msg: missing)
    L.config.handleConfigEvent(e, instLoc())

template handleWriteConf(L: Lexer, cfg: string): untyped =
  {.line.}:
    let e = ConfigFileEvent(kind: cekWriteConfig,
                            instLoc: instLoc(),
                            msg: cfg)
    L.config.handleConfigEvent(e, instLoc())

template handleTrace(L: Lexer, trace: string): untyped =
  {.line.}:
    let e = ConfigFileEvent(kind: cekDebugTrace,
                            location: L.getLineInfo,
                            instLoc: instLoc(),
                            msg: trace)
    L.config.handleConfigEvent(e, instLoc())

template handleRead(conf: ConfigRef,
                    evt: range[cekDebugReadStart..cekProgressConfStart],
                    filename: string): untyped =
  {.line.}:
    let e = ConfigFileEvent(kind: evt,
                            instLoc: instLoc(),
                            msg: filename)
    conf.handleConfigEvent(e, instLoc())

proc ppGetTok(L: var Lexer, tok: var Token) =
  # simple filter
  rawGetTok(L, tok)
  while tok.tokType in {tkComment}: rawGetTok(L, tok)

proc parseExpr(L: var Lexer, tok: var Token; config: ConfigRef): bool
proc parseAtom(L: var Lexer, tok: var Token; config: ConfigRef): bool =
  if tok.tokType == tkParLe:
    ppGetTok(L, tok)
    result = parseExpr(L, tok, config)
    if tok.tokType == tkParRi:
      ppGetTok(L, tok)
    else:
      handleError(L, cekParseExpectedCloseX, ")")
      # localReport(L, LexerReport(kind: rlexExpectedToken, msg: ")"))

  elif tok.tokType == tkNot:
    ppGetTok(L, tok)
    result = not parseAtom(L, tok, config)
  else:
    result = isDefined(config, tok.ident.s)
    ppGetTok(L, tok)

proc parseAndExpr(L: var Lexer, tok: var Token; config: ConfigRef): bool =
  result = parseAtom(L, tok, config)
  while tok.tokType == tkAnd:
    ppGetTok(L, tok)          # skip "and"
    var b = parseAtom(L, tok, config)
    result = result and b

proc parseExpr(L: var Lexer, tok: var Token; config: ConfigRef): bool =
  result = parseAndExpr(L, tok, config)
  while tok.tokType == tkOr:
    ppGetTok(L, tok)          # skip "or"
    var b = parseAndExpr(L, tok, config)
    result = result or b

proc evalppIf(L: var Lexer, tok: var Token; config: ConfigRef): bool =
  ppGetTok(L, tok)            # skip 'if' or 'elif'
  result = parseExpr(L, tok, config)
  if tok.tokType == tkColon:
    ppGetTok(L, tok)
  else:
    handleExpectedX(L, ":")
    # localReport(L, LexerReport(kind: rlexExpectedToken, msg: ":"))

#var condStack: seq[bool] = @[]

proc doEnd(L: var Lexer, tok: var Token; condStack: var seq[bool]) =
  if high(condStack) < 0:
    handleExpectedX(L, "@if")
    # localReport(L, LexerReport(kind: rlexExpectedToken, msg: "@if"))

  ppGetTok(L, tok)            # skip 'end'
  setLen(condStack, high(condStack))

type
  TJumpDest = enum
    jdEndif, jdElseEndif

proc jumpToDirective(L: var Lexer, tok: var Token, dest: TJumpDest; config: ConfigRef;
                     condStack: var seq[bool])
proc doElse(L: var Lexer, tok: var Token; config: ConfigRef; condStack: var seq[bool]) =
  if high(condStack) < 0:
    handleExpectedX(L, "@if")
    # localReport(L, LexerReport(kind: rlexExpectedToken, msg: "@if"))

  ppGetTok(L, tok)
  if tok.tokType == tkColon:
    ppGetTok(L, tok)

  if condStack[high(condStack)]:
    jumpToDirective(L, tok, jdEndif, config, condStack)

proc doElif(L: var Lexer, tok: var Token; config: ConfigRef; condStack: var seq[bool]) =
  if high(condStack) < 0:
    handleExpectedX(L, "@if")
    # localReport(L, LexerReport(kind: rlexExpectedToken, msg: "@if"))

  var res = evalppIf(L, tok, config)
  if condStack[high(condStack)] or not res:
    jumpToDirective(L, tok, jdElseEndif, config, condStack)

  else:
    condStack[high(condStack)] = true

proc jumpToDirective(L: var Lexer, tok: var Token, dest: TJumpDest; config: ConfigRef;
                     condStack: var seq[bool]) =
  var nestedIfs = 0
  while true:
    if tok.ident != nil and tok.ident.s == "@":
      ppGetTok(L, tok)
      case whichKeyword(tok.ident)
      of wIf:
        inc(nestedIfs)
      of wElse:
        if dest == jdElseEndif and nestedIfs == 0:
          doElse(L, tok, config, condStack)
          break
      of wElif:
        if dest == jdElseEndif and nestedIfs == 0:
          doElif(L, tok, config, condStack)
          break
      of wEnd:
        if nestedIfs == 0:
          doEnd(L, tok, condStack)
          break
        if nestedIfs > 0: dec(nestedIfs)
      else:
        discard
      ppGetTok(L, tok)
    elif tok.tokType == tkEof:
      handleExpectedX(L, "@end")
      # localReport(L, LexerReport(kind: rlexExpectedToken, msg: "@end"))
    else:
      ppGetTok(L, tok)

proc parseDirective(L: var Lexer, tok: var Token; config: ConfigRef; condStack: var seq[bool]) =
  ppGetTok(L, tok)            # skip @
  case whichKeyword(tok.ident)
  of wIf:
    setLen(condStack, condStack.len + 1)
    let res = evalppIf(L, tok, config)
    condStack[high(condStack)] = res
    if not res: jumpToDirective(L, tok, jdElseEndif, config, condStack)
  of wElif: doElif(L, tok, config, condStack)
  of wElse: doElse(L, tok, config, condStack)
  of wEnd: doEnd(L, tok, condStack)
  of wWrite:
    ppGetTok(L, tok)
    # TODO: replace this
    L.handleWriteConf:
      strtabs.`%`($tok, config.configVars, {useEnvironment, useKey})
    # L.localReport(InternalReport(
    #   kind: rintNimconfWrite,
    #   msg: strtabs.`%`($tok, config.configVars, {useEnvironment, useKey})))

    ppGetTok(L, tok)
  else:
    case tok.ident.s.normalize
    of "putenv":
      ppGetTok(L, tok)
      var key = $tok
      ppGetTok(L, tok)
      os.putEnv(key, $tok)
      ppGetTok(L, tok)

    of "prependenv":
      ppGetTok(L, tok)
      var key = $tok
      ppGetTok(L, tok)
      os.putEnv(key, $tok & os.getEnv(key))
      ppGetTok(L, tok)

    of "appendenv":
      ppGetTok(L, tok)
      var key = $tok
      ppGetTok(L, tok)
      os.putEnv(key, os.getEnv(key) & $tok)
      ppGetTok(L, tok)

    of "trace":
      ppGetTok(L, tok)
      L.handleTrace($tok)
      # localReport(L, DebugReport(kind: rdbgCfgTrace, str: $tok))
      ppGetTok(L, tok)

    else:
      handleError(L, cekInvalidDirective, $tok)
      # localReport(L, LexerReport(kind: rlexCfgInvalidDirective, msg: $tok))

proc confTok(L: var Lexer, tok: var Token; config: ConfigRef; condStack: var seq[bool]) =
  ppGetTok(L, tok)
  while tok.ident != nil and tok.ident.s == "@":
    parseDirective(L, tok, config, condStack)    # else: give the token to the parser

proc checkSymbol(L: Lexer, tok: Token) =
  if tok.tokType notin {tkSymbol..tkInt64Lit, tkStrLit..tkTripleStrLit}:
    handleError(L, cekParseExpectedIdent, $tok)
    # localReport(L, ParserReport(kind: rparIdentExpected, msg: $tok))

proc parseAssignment(L: var Lexer, tok: var Token;
                     config: ConfigRef; condStack: var seq[bool]) =
  if tok.ident != nil:
    if tok.ident.s == "-" or tok.ident.s == "--":
      confTok(L, tok, config, condStack)           # skip unnecessary prefix
  var info = getLineInfo(L, tok) # save for later in case of an error
  checkSymbol(L, tok)
  var s = $tok
  confTok(L, tok, config, condStack)             # skip symbol
  var val = ""
  while tok.tokType == tkDot:
    s.add('.')
    confTok(L, tok, config, condStack)
    checkSymbol(L, tok)
    s.add($tok)
    confTok(L, tok, config, condStack)
  if tok.tokType == tkBracketLe:
    # BUGFIX: val, not s!
    confTok(L, tok, config, condStack)
    checkSymbol(L, tok)
    val.add('[')
    val.add($tok)
    confTok(L, tok, config, condStack)

    if tok.tokType == tkBracketRi:
      confTok(L, tok, config, condStack)
    else:
      handleError(L, cekParseExpectedCloseX, "]")
      # localReport(L, LexerReport(kind: rlexExpectedToken, msg: "]"))

    val.add(']')
  let percent = tok.ident != nil and tok.ident.s == "%="
  if tok.tokType in {tkColon, tkEquals} or percent:
    if val.len > 0: val.add(':')
    confTok(L, tok, config, condStack)           # skip ':' or '=' or '%'
    checkSymbol(L, tok)
    val.add($tok)
    confTok(L, tok, config, condStack)           # skip symbol
    if tok.tokType in {tkColon, tkEquals}:
      val.add($tok) # add the :
      confTok(L, tok, config, condStack)           # skip symbol
      checkSymbol(L, tok)
      val.add($tok) # add the token after it
      confTok(L, tok, config, condStack)           # skip symbol
    while tok.ident != nil and tok.ident.s == "&":
      confTok(L, tok, config, condStack)
      checkSymbol(L, tok)
      val.add($tok)
      confTok(L, tok, config, condStack)
  if percent:
    processSwitch(s, strtabs.`%`(val, config.configVars,
                                {useEnvironment, useEmpty}), passPP, info, config)
  else:
    processSwitch(s, val, passPP, info, config)

proc readConfigFile*(filename: AbsoluteFile; cache: IdentCache;
                    config: ConfigRef): bool =
  var
    L: Lexer
    tok: Token
    stream: PLLStream

  stream = llStreamOpen(filename, fmRead)
  if stream != nil:
    config.handleRead(cekDebugReadStart, filename.string)
    # config.localReport DebugReport(
    #   kind: rdbgStartingConfRead,
    #   filename: filename.string
    # )

    initToken(tok)
    openLexer(L, filename, stream, cache, config)
    tok.tokType = tkEof       # to avoid a pointless warning
    var condStack: seq[bool] = @[]
    confTok(L, tok, config, condStack)           # read in the first token
    while tok.tokType != tkEof: parseAssignment(L, tok, config, condStack)
    if condStack.len > 0:
      handleError(L, cekParseExpectedX, "@end")
      # localReport(L, LexerReport(kind: rlexExpectedToken, msg: "@end"))
    closeLexer(L)

    config.handleRead(cekDebugReadStop, filename.string)
    # config.localReport DebugReport(
    #   kind: rdbgFinishedConfRead,
    #   filename: filename.string
    # )

    return true

proc getUserConfigPath*(filename: RelativeFile): AbsoluteFile =
  result = getConfigDir().AbsoluteDir / RelativeDir"nim" / filename

proc getSystemConfigPath*(conf: ConfigRef; filename: RelativeFile): AbsoluteFile =
  # try standard configuration file (installation did not distribute files
  # the UNIX way)
  let p = getPrefixDir(conf)
  result = p / RelativeDir"config" / filename
  when defined(unix):
    if not fileExists(result): result = p / RelativeDir"etc/nim" / filename
    if not fileExists(result): result = AbsoluteDir"/etc/nim" / filename

proc loadConfigs*(
    cfg: RelativeFile; cache: IdentCache;
    conf: ConfigRef; idgen: IdGenerator
  ) =
  setDefaultLibpath(conf)
  proc readConfigFile(path: AbsoluteFile) =
    let configPath = path
    if readConfigFile(configPath, cache, conf):
      conf.configFiles.add(configPath)

  proc runNimScriptIfExists(path: AbsoluteFile, isMain = false) =
    let p = path # eval once
    var s: PLLStream
    if isMain and optWasNimscript in conf.globalOptions:
      if conf.projectIsStdin:
        s = stdin.llStreamOpen

      elif conf.projectIsCmd:
        s = llStreamOpen(conf.cmdInput)

    if s == nil and fileExists(p):
      s = llStreamOpen(p, fmRead)

    if s != nil:
      conf.configFiles.add(p)
      runNimScript(cache, p, idgen, freshDefines = false, conf, s)


  if optSkipSystemConfigFile notin conf.globalOptions:
    readConfigFile(getSystemConfigPath(conf, cfg))

    if cfg == DefaultConfig:
      runNimScriptIfExists(getSystemConfigPath(conf, DefaultConfigNims))


  if optSkipUserConfigFile notin conf.globalOptions:
    readConfigFile(getUserConfigPath(cfg))

    if cfg == DefaultConfig:
      runNimScriptIfExists(getUserConfigPath(DefaultConfigNims))

  let pd = if not conf.projectPath.isEmpty:
             conf.projectPath
           else:
             AbsoluteDir(getCurrentDir())


  if optSkipParentConfigFiles notin conf.globalOptions:
    for dir in parentDirs(pd.string, fromRoot=true, inclusive=false):
      readConfigFile(AbsoluteDir(dir) / cfg)

      if cfg == DefaultConfig:
        runNimScriptIfExists(AbsoluteDir(dir) / DefaultConfigNims)

  if optSkipProjConfigFile notin conf.globalOptions:
    readConfigFile(pd / cfg)
    if cfg == DefaultConfig:
      runNimScriptIfExists(pd / DefaultConfigNims)

    if conf.projectName.len != 0:
      # new project wide config file:
      var projectConfig = changeFileExt(conf.projectFull, "nimcfg")
      if not fileExists(projectConfig):
        projectConfig = changeFileExt(conf.projectFull, "nim.cfg")
      readConfigFile(projectConfig)


  let scriptFile = conf.projectFull.changeFileExt("nims")
  let scriptIsProj = scriptFile == conf.projectFull
  template showHintConf =
    for filename in conf.configFiles:
      # delayed to here so that `hintConf` is honored
      conf.handleRead(cekProgressConfStart, filename.string)
      # localReport(conf, ExternalReport(kind: rextConf, msg: filename.string))
  if conf.cmd == cmdNimscript:
    showHintConf()
    conf.configFiles.setLen 0
  if conf.cmd != cmdIdeTools:
    if conf.cmd == cmdNimscript:
      runNimScriptIfExists(conf.projectFull, isMain = true)
    else:
      runNimScriptIfExists(scriptFile, isMain = true)
  else:
    if not scriptIsProj:
      runNimScriptIfExists(scriptFile, isMain = true)
    else:
      # 'nimsuggest foo.nims' means to just auto-complete the NimScript file
      discard
  showHintConf()
