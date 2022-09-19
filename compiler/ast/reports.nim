## This module provides type definitions for all structured report entries
## that compiler can provide.
##
## Note that this module specifically does not import anything else from
## the compiler - by design it is supposed to be available in every other
## module (because almost any phase of the compiler can generate reports
## one way or another). By design report should contain as much information
## as possible and *never* be used for any conditional logic in the
## compiler - it is a final form of the output that can only be printed to
## the output (either via user-provided report hook implementation, or
## using one of the built-in ones)
##
## Not using compiler-specific types also allows this report to be easily
## reused by external tooling - custom error pretty-printers, test runners
## and so on.
## 
## Debug Defines:
## `compilerDebugCompilerReportStatistics`: output stats of counts for various
##                                          report kinds

import std/[options, packedsets]

import
  compiler/vm/vm_enums,
  compiler/ast/ast_types,
  compiler/utils/[int128, platform]

export
  ast_types,
  options.some,
  options.none,
  options.Option,
  int128.toInt128

from compiler/front/in_options import TOption, TOptions
type InstantiationInfo* = typeof(instantiationInfo())

# Importing and reexporting enums and 'external' reports in order to avoid
# needlessly cluttering the import lists of all modules that have to report
# something (and that would be almost all modules)
import report_enums
export report_enums

type
  LexerReport* = object of ReportBase
    msg*: string
    case kind*: ReportKind
      of rlexLinterReport:
        wanted*: string
        got*: string
      else:
        discard

func severity*(rep: LexerReport): ReportSeverity =
  case LexerReportKind(rep.kind):
    of rlexHintKinds: rsevHint
    of rlexErrorKinds: rsevError
    of rlexWarningKinds: rsevWarning


type
  ParserReport* = object of ReportBase
    msg*: string
    found*: string
    case kind*: ReportKind
      of rparIdentExpected .. rparUnexpectedToken:
        expected*: seq[string]

      of rparInvalidFilter:
        node*: PNode

      else:
        discard

func severity*(parser: ParserReport): ReportSeverity =
  case ParserReportKind(parser.kind):
    of rparHintKinds: rsevHint
    of rparWarningKinds: rsevWarning
    of rparErrorKinds: rsevError


func severity*(report: SemReport): ReportSeverity =
  case SemReportKind(report.kind):
    of rsemErrorKinds:   result = rsevError
    of rsemWarningKinds: result = rsevWarning
    of rsemHintKinds:    result = rsevHint
    of rsemFatalError:   result = rsevFatal

proc reportSymbols*(
    kind: ReportKind,
    symbols: seq[PSym],
    typ: PType = nil,
    ast: PNode = nil
  ): SemReport =
  case kind:
    of rsemReportTwoSym: assert symbols.len == 2
    of rsemReportOneSym: assert symbols.len == 1
    of rsemReportListSym: discard
    else: assert false, $kind

  result = SemReport(kind: kind, ast: ast)
  result.symbols = symbols
  result.typ = typ

func reportSem*(kind: ReportKind): SemReport = SemReport(kind: kind)

func reportAst*(
    kind: ReportKind,
    ast: PNode, str: string = "", typ: PType = nil, sym: PSym = nil
  ): SemReport =

  SemReport(kind: kind, ast: ast, str: str, typ: typ, sym: sym)

func reportTyp*(
    kind: ReportKind,
    typ: PType, ast: PNode = nil, sym: PSym = nil, str: string = ""
  ): SemReport =
  SemReport(kind: kind, typ: typ, ast: ast, sym: sym, str: str)

func reportStr*(
    kind: ReportKind,
    str: string, ast: PNode = nil, typ: PType = nil, sym: PSym = nil
  ): SemReport =

  SemReport(kind: kind, ast: ast, str: str, typ: typ, sym: sym)

func reportSym*(
    kind: ReportKind,
    sym: PSym, ast: PNode = nil, str: string = "", typ: PType = nil,
  ): SemReport =

  SemReport(kind: kind, ast: ast, str: str, typ: typ, sym: sym)

func severity*(vm: VMReport): ReportSeverity =
  case VMReportKind(vm.kind):
    of rvmStackTrace:    result = rsevTrace
    else: result = rsevError



template withIt*(expr: untyped, body: untyped): untyped =
  block:
    var it {.inject.} = expr
    body
    it

template tern*(predicate: bool, tBranch: untyped, fBranch: untyped): untyped =
  ## Shorthand for inline if/else. Allows use of conditions in strformat,
  ## simplifies use in expressions. Less picky with formatting
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      if predicate: tBranch else: fBranch

type
  CmdReport* = object of ReportBase
    cmd*: string
    msg*: string
    code*: int
    case kind*: ReportKind
      of rcmdFailedExecution:
        exitOut*, exitErr*: string

      else:
        discard

func severity*(report: CmdReport): ReportSeverity =
  case CmdReportKind(report.kind):
    of rcmdHintKinds: rsevHint
    of rcmdWarningKinds: rsevWarning
    of rcmdErrorKinds: rsevError

type
  DebugSemStepDirection* = enum semstepEnter, semstepLeave
  DebugSemStepKind* = enum
    stepNodeToNode
    stepNodeToSym
    stepIdentToSym
    stepSymNodeToNode
    stepNodeFlagsToNode
    stepNodeTypeToNode
    stepTypeTypeToType
    stepResolveOverload
    stepNodeSigMatch
    stepWrongNode
    stepError
    stepTrack

  DebugCallableCandidate* = object
    ## stripped down version of `sigmatch.TCandidate`
    state*: string
    callee*: PType
    calleeSym*: PSym
    calleeScope*: int
    call*: PNode
    error*: SemCallMismatch

  DebugSemStep* = object
    direction*: DebugSemStepDirection
    level*: int
    name*: string
    node*: PNode ## Depending on the step direction this field stores
                 ## either input or output node
    steppedFrom*: ReportLineInfo
    sym*: PSym

    case kind*: DebugSemStepKind
      of stepIdentToSym:
        ident*: PIdent

      of stepNodeTypeToNode, stepTypeTypeToType:
        typ*: PType
        typ1*: PType


      of stepNodeFlagsToNode:
        flags*: TExprFlags
      
      of stepNodeSigMatch, stepResolveOverload:
        filters*: TSymKinds
        candidate*: DebugCallableCandidate
        errors*: seq[SemCallMismatch]

      else:
        discard

  DebugVmCodeEntry* = object
    isTarget*: bool
    info*: TLineInfo
    pc*: int
    idx*: int
    case opc*: TOpcode:
      of opcConv, opcCast:
        types*: tuple[tfrom, tto: PType]
      of opcLdConst, opcAsgnConst:
        ast*: PNode
      else:
        discard
    ra*: int
    rb*: int
    rc*: int

  DebugReport* = object of ReportBase
    case kind*: ReportKind
      of rdbgOptionsPush, rdbgOptionsPop:
        optionsNow*: TOptions

      of rdbgVmExecTraceFull:
        vmgenExecFull*: tuple[
          pc: int,
          opc: TOpcode,
          info: TLineInfo,
          ra, rb, rc: TRegisterKind
        ]

      of rdbgTraceStep:
        semstep*: DebugSemStep

      of rdbgTraceLine, rdbgTraceStart:
        ctraceData*: tuple[level: int, entries: seq[StackTraceEntry]]

      of rdbgStartingConfRead, rdbgFinishedConfRead:
        filename*: string

      of rdbgCfgTrace:
        str*: string

      of rdbgVmCodeListing:
        vmgenListing*: tuple[
          sym: PSym,
          ast: PNode,
          entries: seq[DebugVmCodeEntry]
        ]

      of rdbgVmExecTraceMinimal:
        vmgenExecMinimal*: tuple[
          info: TLineInfo,
          opc: TOpcode
        ]

      else:
        discard

func severity*(report: DebugReport): ReportSeverity =
  rsevDebug

type
  BackendReport* = object of ReportBase
    msg*: string
    usedCompiler*: string
    case kind*: ReportKind
      of rbackCannotWriteScript,
         rbackProducedAssembly,
         rbackCannotWriteMappingFile:
        filename*: string

      of rbackTargetNotSupported:
        requestedTarget*: string

      of rbackJsonScriptMismatch:
        jsonScriptParams*: tuple[
          outputCurrent, output, jsonFile: string
        ]

      of rbackVmFileWriteFailed:
        outFilename*: string
        failureMsg*: string ## string rep of the ``RodFileError``, so that
                           ## ``rodfiles`` doesn't need to be imported here

      else:
        discard

func severity*(report: BackendReport): ReportSeverity =
  case BackendReportKind(report.kind):
    of rbackErrorKinds: rsevError
    of rbackHintKinds: rsevHint
    of rbackWarningKinds: rsevWarning

type
  ExternalReport* = object of ReportBase
    ## Report about external environment reads, passed configuration
    ## options etc.
    msg*: string

    case kind*: ReportKind
      of rextInvalidHint .. rextInvalidPath:
        cmdlineSwitch*: string ## Switch in processing
        cmdlineProvided*: string ## Value passed to the command-line
        cmdlineAllowed*: seq[string] ## Allowed command-line values
        cmdlineError*: string ## Textual description of the cmdline failure

      of rextUnknownCCompiler:
        knownCompilers*: seq[string]
        passedCompiler*: string

      of rextInvalidPackageName:
        packageName*: string

      of rextPath:
        packagePath*: string

      else:
        discard

func severity*(report: ExternalReport): ReportSeverity =
  case ExternalReportKind(report.kind):
    of rextErrorKinds: rsevError
    of rextWarningKinds: rsevWarning
    of rextHintKinds: rsevHint

type
  UsedBuildParams* = object
    project*: string
    output*: string
    linesCompiled*: int
    mem*: int
    isMaxMem*: bool
    sec*: float
    case isCompilation*: bool
      of true:
        threads*: bool
        backend*: string
        buildMode*: string
        optimize*: string
        gc*: string

      of false:
        discard

  InternalStateDump* = ref object
    version*: string
    nimExe*: string
    prefixdir*: string
    libpath*: string
    projectPath*: string
    definedSymbols*: seq[string]
    libPaths*: seq[string]
    lazyPaths*: seq[string]
    nimbleDir*: string
    outdir*: string
    `out`*: string
    nimcache*: string
    hints*, warnings*: seq[tuple[name: string, enabled: bool]]

  InternalCliData* = object
    ## Information used to construct messages for CLI reports - `--help`,
    ## `--fullhelp`
    version*: string ## Language version
    sourceHash*: string ## Compiler source code git hash
    sourceDate*: string ## Compiler source code date
    boot*: seq[string] ## nim compiler boot flags
    cpu*: TSystemCPU ## Target CPU
    os*: TSystemOS ## Target OS

  InternalReport* = object of ReportBase
    ## Report generated for the internal compiler workings
    msg*: string
    case kind*: ReportKind
      of rintStackTrace:
        trace*: seq[StackTraceEntry] ## Generated stack trace entries

      of rintDumpState:
        stateDump*: InternalStateDump

      of rintAssert:
        expression*: string

      of rintSuccessX:
        buildParams*: UsedBuildParams

      of rintCannotOpenFile .. rintWarnFileChanged:
        file*: string

      of rintListWarnings, rintListHints:
        enabledOptions*: set[ReportKind]

      of rintCliKinds:
        cliData*: InternalCliData

      else:
        discard

func severity*(report: InternalReport): ReportSeverity =
  case InternalReportKind(report.kind):
    of rintFatalKinds:    rsevFatal
    of rintHintKinds:     rsevHint
    of rintWarningKinds:  rsevWarning
    of rintErrorKinds:    rsevError
    of rintDataPassKinds: rsevTrace


type
  ReportTypes* =
    LexerReport    |
    ParserReport   |
    SemReport      |
    VMReport       |
    CmdReport      |
    DebugReport    |
    InternalReport |
    BackendReport  |
    ExternalReport

  Report* = object
    ## Toplevel wrapper type for the compiler report
    case category*: ReportCategory
      of repLexer:
        lexReport*: LexerReport

      of repParser:
        parserReport*: ParserReport

      of repSem:
        semReport*: SemReport

      of repVM:
        vmReport*: VMReport

      of repCmd:
        cmdReport*: CmdReport

      of repDebug:
        debugReport*: DebugReport

      of repInternal:
        internalReport*: InternalReport

      of repBackend:
        backendReport*: BackendReport

      of repExternal:
        externalReport*: ExternalReport

static:
  when defined(compilerDebugCompilerReportStatistics):
    echo(
      "Nimskull compiler outputs ",
      ord(high(ReportKind) + 1),
      " different kinds of diagnostics")

    echo "size of ReportBase     ", sizeof(ReportBase)
    echo "size of LexerReport    ", sizeof(LexerReport)
    echo "size of ParserReport   ", sizeof(ParserReport)
    echo "size of SemReport      ", sizeof(SemReport)
    echo "size of CmdReport      ", sizeof(CmdReport)
    echo "size of DebugReport    ", sizeof(DebugReport)
    echo "size of InternalReport ", sizeof(InternalReport)
    echo "size of BackendReport  ", sizeof(BackendReport)
    echo "size of ExternalReport ", sizeof(ExternalReport)
    echo "size of Report         ", sizeof(Report)
    echo "sem reports      = ", len(repSemKinds)
    echo "lexer reports    = ", len(repLexerKinds)
    echo "parser reports   = ", len(repParserKinds)
    echo "internal reports = ", len(repInternalKinds)

let reportEmpty* = Report(
  category: repInternal,
  internalReport: InternalReport(kind: repNone))

template eachCategory*(report: Report, field: untyped): untyped =
  case report.category:
    of repLexer:    report.lexReport.field
    of repParser:   report.parserReport.field
    of repCmd:      report.cmdReport.field
    of repVM:       report.vmReport.field
    of repSem:      report.semReport.field
    of repDebug:    report.debugReport.field
    of repInternal: report.internalReport.field
    of repBackend:  report.backendReport.field
    of repExternal: report.externalReport.field

func kind*(report: Report): ReportKind =
  eachCategory(report, kind)

func location*(report: Report): Option[TLineInfo] =
  eachCategory(report, location)

func reportInst*(report: Report): ReportLineInfo =
  eachCategory(report, reportInst)

func reportFrom*(report: Report): ReportLineInfo =
  eachCategory(report, reportFrom)

func context*(report: Report): seq[ReportContext] =
  # refactor: this shouldn't be on report
  case report.category:
    of repVM:  report.vmReport.context
    of repSem: report.semReport.context
    else:      @[]

func `context=`*(report: var Report, context: seq[ReportContext]) =
  # refactor: this shouldn't be on report
  case report.category:
    of repSem: report.semReport.context = context
    of repVM:  report.vmReport.context = context
    else:      discard

func `reportFrom=`*(report: var Report, loc: ReportLineInfo) =
  case report.category:
    of repLexer:    report.lexReport.reportFrom = loc
    of repParser:   report.parserReport.reportFrom = loc
    of repCmd:      report.cmdReport.reportFrom = loc
    of repSem:      report.semReport.reportFrom = loc
    of repVM:       report.vmReport.reportFrom = loc
    of repDebug:    report.debugReport.reportFrom = loc
    of repInternal: report.internalReport.reportFrom = loc
    of repBackend:  report.backendReport.reportFrom = loc
    of repExternal: report.externalReport.reportFrom = loc

func category*(kind: ReportKind): ReportCategory =
  case kind:
    of repDebugKinds:    result = repDebug
    of repInternalKinds: result = repInternal
    of repExternalKinds: result = repExternal
    of repCmdKinds:      result = repCmd

    of repLexerKinds:    result = repLexer
    of repParserKinds:   result = repParser
    of repSemKinds:      result = repSem
    of repBackendKinds:  result = repBackend
    of repVMKinds:       result = repVM

    of repNone: assert false, "'none' report does not have category"

func severity*(
    report: ReportTypes,
    asError: ReportKinds,
    asWarning: ReportKinds = default(ReportKinds)
  ): ReportSeverity =

  if report.kind in asError:
    rsevError

  elif report.kind in asWarning:
    rsevWarning

  else:
    severity(report)

func severity*(
    report: Report,
    asError: ReportKinds = default(ReportKinds),
    asWarning: ReportKinds = default(ReportKinds)
  ): ReportSeverity =
  ## Return report severity accounting for 'asError' and 'asWarning'
  ## mapping sets.

  if report.kind in asError: rsevError
  elif report.kind in asWarning: rsevWarning
  else:
    case report.category:
      of repLexer:    report.lexReport.severity()
      of repParser:   report.parserReport.severity()
      of repSem:      report.semReport.severity()
      of repCmd:      report.cmdReport.severity()
      of repVM:       report.vmReport.severity()
      of repInternal: report.internalReport.severity()
      of repBackend:  report.backendReport.severity()
      of repDebug:    report.debugReport.severity()
      of repExternal: report.externalReport.severity()

func toReportLineInfo*(iinfo: InstantiationInfo): ReportLineInfo =
  ReportLineInfo(file: iinfo[0], line: uint16(iinfo[1]), col: int16(iinfo[2]))

template calledFromInfo*(): ReportLineInfo =
  {.line.}:
    let e = getStackTraceEntries()[^2]
    ReportLineInfo(file: $e.filename, line: e.line.uint16)

func isValid*(point: ReportLineInfo): bool =
  0 < point.file.len and point.file != "???"

template reportHere*[R: ReportTypes](report: R): R =
  block:
    var tmp = report
    tmp.reportInsta = toReportLineInfo(
      instantiationInfo(fullPaths = true))

    tmp

func wrap*(rep: sink LexerReport): Report =
  assert rep.kind in repLexerKinds, $rep.kind
  Report(category: repLexer, lexReport: rep)

func wrap*(rep: sink ParserReport): Report =
  assert rep.kind in repParserKinds, $rep.kind
  Report(category: repParser, parserReport: rep)

func wrap*(rep: sink VMReport): Report =
  assert rep.kind in repVMKinds, $rep.kind
  Report(category: repVM, vmReport: rep)

func wrap*(rep: sink SemReport): Report =
  assert rep.kind in repSemKinds, $rep.kind
  Report(category: repSem, semReport: rep)

func wrap*(rep: sink BackendReport): Report =
  assert rep.kind in repBackendKinds, $rep.kind
  Report(category: repBackend, backendReport: rep)

func wrap*(rep: sink CmdReport): Report =
  assert rep.kind in repCmdKinds, $rep.kind
  Report(category: repCmd, cmdReport: rep)

func wrap*(rep: sink DebugReport): Report =
  assert rep.kind in repDebugKinds, $rep.kind
  Report(category: repDebug, debugreport: rep)

func wrap*(rep: sink InternalReport): Report =
  assert rep.kind in repInternalKinds, $rep.kind
  Report(category: repInternal, internalReport: rep)

func wrap*(rep: sink ExternalReport): Report =
  assert rep.kind in repExternalKinds, $rep.kind
  Report(category: repExternal, externalReport: rep)

func wrap*[R: ReportTypes](rep: sink R, iinfo: InstantiationInfo): Report =
  var tmp = rep
  tmp.reportInst = toReportLineInfo(iinfo)
  return wrap(tmp)

func wrap*[R: ReportTypes](
    rep: sink R, iinfo: ReportLineInfo, point: TLineInfo): Report =
  var tmp = rep
  tmp.reportInst = iinfo
  tmp.location = some point
  return wrap(tmp)

func wrap*[R: ReportTypes](
    rep: sink R, iinfo: InstantiationInfo, point: TLineInfo): Report =
  wrap(rep, toReportLineInfo(iinfo), point)

func wrap*[R: ReportTypes](iinfo: InstantiationInfo, rep: sink R): Report =
  wrap(rep, iinfo)

template wrap*(rep: ReportTypes): Report =
  wrap(rep, toReportLineInfo(instLoc()))


func `$`*(point: ReportLineInfo): string =
  point.file & "(" & $point.line & ", " & $point.col & ")"


type
  ReportList* = object
    ## List of the accumulated reports. Used for various `sem*` reporting
    ## mostly, and in other places where report might be *generated*, but
    ## not guaranteed to be printed out.
    list: seq[Report]

  ReportSet* = object
    ids: PackedSet[uint32]

func incl*(s: var ReportSet, id: ReportId) = s.ids.incl uint32(id)
func contains*(s: var ReportSet, id: ReportId): bool = s.ids.contains uint32(id)

func addReport*(list: var ReportList, report: sink Report): ReportId =
  ## Add report to the report list
  list.list.add report
  result = ReportId(uint32(list.list.high) + 1)

func addReport*[R: ReportTypes](list: var ReportList, report: R): ReportId =
  addReport(list, wrap(report))

func getReport*(list: ReportList, id: ReportId): Report =
  ## Get report from the report list using it's id
  list.list[int(uint32(id)) - 1]


func actualType*(r: SemReport | VMReport): PType = r.typeMismatch[0].actualType
func formalType*(r: SemReport | VMReport): PType = r.typeMismatch[0].formalType
func formalTypeKind*(r: SemReport): set[TTypeKind] = r.typeMismatch[0].formalTypeKind
func symstr*(r: SemReport | VMReport): string = r.sym.name.s
