## Not really a "new AST", just not sure what to call it yet
## 
## Finding your way around:
## - `SyntaxKind/Datum/ParsedSource` and friends are for a compact AST
## - there is a comment `INTERPRETER-AS-COMPILER STARTS HERE`, that's where the
##   good(?) stuff starts


type
  # xxx: resort these based on data storage, or the type of syntax node? Maybe
  #      keep consistent with 
  SyntaxKind = enum
    synEmpty
    synIdent
    synCharLit
    synIntLit
    synInt8Lit
    synInt16Lit
    synInt32Lit
    synInt64Lit
    synUIntLit
    synUInt8Lit
    synUInt16Lit
    synUInt32Lit
    synUInt64Lit
    synFloatLit
    synFloat32Lit
    synFloat64Lit
    synFloat128Lit
    synStrLit
    synRStrLit
    synTripleStrLit
    synNilLit
    synCustomLit
    synAccQuoted
    synCall
    synCommand
    synCallStrLit
    synInfix
    synPrefix
    synPostfix
    synExprEqExpr
    synExprColonExpr
    synIdentDefs
    synConstDef
    synVarTuple
    synPar
    synSqrBracket
    synCurly
    synTupleConstr
    synObjConstr
    synTableConstr
    synSqrBracketExpr
    synCurlyExpr
    synPragmaExpr
    synPragma
    synPragmaBlock
    synDotExpr
    synIfExpr
    synIfStmt
    synElifBranch
    synElifExpr
    synElse
    synElseExpr
    synCaseStmt
    synOfBranch
    synWhenExpr
    synWhenStmt
    synForStmt
    synWhileStmt
    synBlockExpr
    synBlockStmt
    synDiscardStmt
    synContinueStmt
    synBreakStmt
    synReturnStmt
    synRaiseStmt
    synYieldStmt
    synTryStmt
    synExceptBranch
    synFinally
    synDefer
    synLambda
    synDo
    synBind
    synBindStmt
    synMixinStmt
    synCast
    synStaticStmt
    synAsgn
    synGenericParams
    synFormalParams
    synStmtList
    synStmtListExpr
    synImportStmt
    synImportExceptStmt
    synFromStmt
    synIncludeStmt
    synExportStmt
    synExportExceptStmt
    synConstSection
    synLetSection
    synVarSection
    synProcDef
    synFuncDef
    synMethodDef
    synConverterDef
    synIteratorDef
    synMacroDef
    synTemplateDef
    synTypeSection
    synTypeDef
    synEnumTy
    synEnumFieldDef
    synObjectTy
    synTupleTy
    synProcTy
    synIteratorTy
    synRecList
    synRecCase
    synRecWhen
    synTypeOfExpr
    synRefTy
    synVarTy
    synPtrTy
    synStaticTy
    synDistinctTy
    synMutableTy
    synTupleClassTy
    synTypeClassTy
    synOfInherit
    synArgList
    synWith
    synWithout
    synAsmStmt
    synCommentStmt
    synUsingStmt

const
  synStrLitKinds = {synStrLit, synRStrLit, synTripleStrLit}
  synLeafKinds = {synTupleClassTy, synCommentStmt}
  synUptoUnaryKinds = {synDiscardStmt, synReturnStmt, synContinueStmt,
                       synBreakStmt, synRaiseStmt, synRefTy, synVarTy,
                       synPtrTy}
    # xxx: split these into dedicated kinds leaf vs unary
  synUnaryKinds = {synCallStrLit, synYieldStmt, synBind, synStaticStmt,
                   synMutableTy}
  synBinaryKinds = {synInfix, synPrefix, synPostfix, synExprEqExpr,
                    synExprColonExpr, synDotExpr, synAsgn, synEnumFieldDef}
  synRoutineDefKinds = {synProcDef, synFuncDef, synMethodDef, synConverterDef,
                        synIteratorDef, synMacroDef, synTemplateDef, synLambda}
    ## known layout of name, generic, pattern, params, pragma, body
  synNKidsKinds = {synAccQuoted, synCall, synCommand, synIdentDefs,
                   synVarTuple, synPar, synSqrBracket, synCurly,
                   synTupleConstr, synObjConstr, synTableConstr,
                   synSqrBracketExpr, synCurlyExpr, synPragmaExpr, synPragma,
                   synPragmaBlock, synIfExpr, synIfStmt, synElifBranch,
                   synElifExpr, synElse, synElseExpr, synCaseStmt, synOfBranch,
                   synWhenExpr, synWhenStmt, synForStmt, synWhileStmt,
                   synBlockExpr, synBlockStmt, synTryStmt, synExceptBranch,
                   synFinally, synDefer, synDo, synBindStmt, synMixinStmt,
                   synCast, synGenericParams, synFormalParams, synStmtList,
                   synStmtListExpr, synImportStmt, synImportExceptStmt,
                   synFromStmt, synIncludeStmt, synExportStmt,
                   synExportExceptStmt, synConstSection, synConstDef,
                   synLetSection, synVarSection, synTypeSection, synTypeDef,
                   synEnumTy, synObjectTy, synTupleTy, synProcTy,
                   synIteratorTy, synRecList, synRecCase, synRecWhen,
                   synTypeOfExpr, synStaticTy, synDistinctTy, synTypeClassTy,
                   synOfInherit, synArgList, synWith, synWithout, synAsmStmt,
                   synUsingStmt}
    ## syntax nodes with 'n' kids (variable width)
    ## xxx: this category can likely be broken down further after fixing the
    ##      rest of the compiler and refining the structures

from compiler/ast/idents import PIdent

type
  # IdentIdx = distinct int32 # xxx: reusing ident cache/PIdent for now
  IdentIdx = PIdent
  StrIdx = distinct int32
  ExtraIdx = distinct int32

  TreeIdx = distinct int32

  SyntaxDatumNode = object
    case kind: SyntaxKind:
      of synEmpty:          discard
      of synIdent:          identId:     IdentIdx   # xxx: rename to symbol?
      of synCharLit:        charLit:     char
      of synIntLit:         intLit:      BiggestInt
      of synInt8Lit:        int8Lit:     int8
      of synInt16Lit:       int16Lit:    int16
      of synInt32Lit:       int32Lit:    int32
      of synInt64Lit:       int64Lit:    int64
      of synUIntLit:        uintLit:     BiggestUInt
      of synUInt8Lit:       uint8Lit:    uint8
      of synUInt16Lit:      uint16Lit:   uint16
      of synUInt32Lit:      uint32Lit:   uint32
      of synUInt64Lit:      uint64Lit:   uint64
      of synFloatLit:       floatLit:    BiggestFloat
      of synFloat32Lit:     float32Lit:  float32
      of synFloat64Lit:     float64Lit:  float64
      of synFloat128Lit:    floa128tLit: BiggestFloat # xxx: this a bug?
      of synStrLitKinds:    strLit:      StrIdx
      of synNilLit:         discard
      of synCustomLit:
                            litPart:     StrIdx
                            suffix:      IdentIdx
      of synUptoUnaryKinds: unary: bool ## if true next node is child
      of synLeafKinds:      discard
      of synUnaryKinds:
        ## next node is the child
        discard
      of synBinaryKinds:
                            rightId: TreeIdx # left always follows the parent
      of synRoutineDefKinds:routineExtraId: ExtraIdx
      of synNKidsKinds:
                            extraId: ExtraIdx
                            count: Natural

  SyntaxDatum = object
    ## an abstract syntax tree as a singular unit of data (datum).
    tree: seq[SyntaxDatumNode]
    extra: seq[TreeIdx]
    # ident: seq[string]       # xxx: reusing ident cache/PIdent for now
    strs: seq[string]

  ParsedSourceId = int32

  ParsedSourceKind = enum
    parserString
    parserFile

  ParsedSource = object
    kind: ParsedSourceKind ## file, string, or whatever
    id: ParsedSourceId     ## opaque id that the receiver should understand

  ParsedSyntax = object
    ## an abstract syntax tree parsed from some string/file
    source: ParsedSource
    syntax: SyntaxDatum

# INTERPRETER-AS-COMPILER STARTS HERE
# TODO:
# 1. generate in-to-out-structions: `nim --compilepreter c foo.nim`
#   - [x] rig up compilepreter into module pass
#   - [ ] write logic and env change events to log
#   - [ ] start seeing it echo
#   - [ ] revise goals
# 2. execute operations against type and sym "environments"
#   - emit more "oustructions"
# 3. sketch types and symbols
#
# Misc TODO:
# - figure a better name that "First"
# - 'outstruction' is very cute... perhaps a better name?
#
# Open Questions:
# - should I generate explicit `feCompileStart`, `feCompileFailed`, etc on a
#   per `fe*` basis, or keep them separate as now?
#
# Risks:
# - need a solid backend target + comptime definition for what PNode/PSym/etc
#   data must be replicated, and to what fidelity
# - I used a struct-of-arrays/vectorized format for logging but, the slightly
#   more annoying to implement upfront, writing of untyped buffers (variable
#   width data) might be a far more pleasant experience for each interpreter

type
  ## various identifiers to point at various pieces of data
  # xxx: need to incorporate once we get to environments
  RunId* = distinct int32
    ## valid values are 0 and above, some state tracking uses values outside
    ## this range (negative numbers), for special casing, if runId was ever
    ## set, the first run ever, etc.

  PhaseId* {.size: sizeof(uint8).} = enum
    phaseFirst    ## whatever `FirstInterpreter` produces
                  # xxx: kinda implies interpreter == level

  UntypedEvtTag* = uint16
    ## the erased event/outstruction enum value, must be sized to 16 bits max

  OutstrStatus* {.pure, size: sizeof(uint8).} = enum
    ## common to all events/outstructions, since everything is meant to be
    ## transactional, this gives a single way to demarcate them consistently.
    started
    successful ## finished successfullly
    failed     ## finished, with errors
    cancelled  ## stopped by the interpreter
    aborted    ## stopped by the caller

  LogEntry* = object
    ## the log needs to be writeable by any interpter, so rather than figuring
    ## out everything upfront in one mega-module, keep a standardize structure
    ## and allow interpreters/modules own their definitions (schema on read).
    # xxx: alternative approach is to change this to an index type and the
    #      actual log is a big ol' byte array which we index into and cast to
    #      pull out variably sized data... not sure what to do, this _seems_
    #      easier to implement now and hopefully similarly easy to work with.
    run*: RunId              # reserving space, ignoring for now
    phase*: PhaseId
    evtStatus*: OutstrStatus
    evtTag*: UntypedEvtTag
    data*: uint64            ## the data, identifier, or index; interpretation
                             ## depends upon the `phase`/`status`/`evtTag`

  InterpreterLogger* = object
    ## logger for the interpreters, we use a single format, extra data is in
    ## look aside buffers that need to be queried at time of read.
    data: seq[LogEntry] # xxx: consider making an array list style structure

type
  InstructionSource* = object
  OutstructionTarget* = object

# import modulegraphs, because it seems `FirstInterpreter` layers over it...
# still figuring out how this should work.
from compiler/utils/idioms import unreachable
# from compiler/modules/modulegraphs import ModuleGraph
  # TODO: this is a recursive dependency, move "FirstInterpreter" out of this module

type
  ## Some types start with "First", because they're the first interpeter we
  ## started defining, and to avoid getting stuck on names from the outset.

  ModuleId* = distinct int32

  CommentId* = int32 # TODO: make distinct

  FirstInstrKind* {.pure.} = enum
    # xxx: this is only used to think about the `legacyXXX` procs and various
    #      steps that are required currently.
    # TODO: refactor around:
    # - rename: 'compile' -> 'project'
    # - project language has 'start/finishProject' (after rename)
    # - module language has 'startfinish/Import'
    # - compile language has 'start/finishCompile'
    # - move 'project' stuff to `modulegraphs`
    # - move module language stuff to `sem`
    # - move compile language to `sem`
    startCompile      ## tie-in with `sem.semPass` and `main` initialization
    processModule     ## tie-in with `sem.myOpen`
    importModule      ## allow auto-injecting `import std/system`
    finishModule      ## tie-in with `sem.myclose`
    finishCompile     ## mostly for the interpreter(s)
    abortCompile      ## blows up everything, interpreter cleans-up
    # ideas:
    # - inject instructions:
    #   - metadata: a no-op with data point or open/close span
    #   - trace: same as metadata, but purely observational
    #   - checkpoint/rollback: custom points to rollback progress

  FirstEvtKind* {.pure.} = enum
    ## these are the "oustructions", each of these are paired with statuses to
    ## get started/success/fail/cancel/etc variants (see `OutstrStatus`).
    # xxx: hmm, if modules is a "high-level" feature, we could delay defining
    #      `feModule`, `feImport`, `feExport` and friends, creating a smaller
    #      core language
    feCompile        ## either the whole project, or a sub-stage
    feDiagnostic     ## hint/warn/error
    feModule         ## module symbol/type
    # more ast oriented processing, ordered by appearance when compile for now
    # this means fancy features are at the end
    feComment        ## no-op/pass-through; can also do doc interpreters
    feImport         ## open module import
    feExport         ## can't import what isn't exported
    feSym            # xxx: not sure if the ident/sym separation is useful
    # literally!
    feCharLit
    feIntLit
    feInt8Lit
    feInt16Lit
    feInt32Lit
    feInt64Lit
    feUIntLit
    feUInt8Lit
    feUInt16Lit
    feUInt32Lit
    feUInt64Lit
    feFloatLit
    feFloat32Lit
    feFloat64Lit
    feFloat128Lit
    feStrLit
    feNilLit
    # operations / lookup
    feDotExpr
    fePrefix
    feInfix          ## call kind for paths
    # expression collections
    fePar
    feStmtList
    feBlock
    # variables
    feVarSection
    feIdentDefs
    feAsgn
    # control flow
    feIf
    feElse
    feWhile
    # types
    feTypeSection
    feTypeDef
    feObjectTy
    feTupleTy
    feRecList         # xxx: repurpose `Rec`/"record" in the language
    feRefTy
    feVarTy
    fePtrTy
    # routines
    feCall
    feProcDef
    feFormalParams
    feReturn
    # fancier control flow
    feContinue
    feBreak
    feDefer
    # product constructors
    feTupleConstr
    feExprColonExpr
    feObjConstr
    feExprEqExpr
    # constructor literals
    feTableConstr
    feSqrBracket
    feCurly
    # constants
    feConstSection
    feConstDef
    # types - enums
    feEnumTy
    feEnumFieldDef
    # more control flow
    feCase
    feOfBranch
    # misc control flow; likely remove
    feElifBranch      # xxx: inclined to remove
    feIfExpr          # xxx: pretty sure we should remove
    feElseExpr        # xxx: pretty sure we should remove
    feElifExpr        # xxx: pretty sure we should remove
    # types - variants
    feRecCase         # xxx: repurpose `Rec`/"record" in the language
    # exceptionally bad ideas
    feTry
    feRaise
    feExcept
    feFinally
    # fancier identifiers
    accQuoted
    # fancier literals
    feRStrLit
    feTripleStrLit
    feCustomLit
    # compilation directives/escape hatches
    feExprPragma       ## nkPragmaExpr, on a sym or whatever
    fePragmaStmt
    feCast
    fePragmaBlock
    feAsmStmt
    # fancier import/export
    feImportExcept
    feExportExcept
    feFrom
    # conditional compilation
    feWhen
    feWhenExpr        # xxx: do we really need this?
    # sugar calls
    feCommand
    feCallStrLit
    fePostFix
    # safety/immutability
    feLetSection
    feDiscard
    feStaticTy
    # constant expression evaluation/static reflection
    feConstEval
    feTypeOf
    # templating
    feIncludeStmt
    feTemplateDef
    feExpandTemplate
    # fancier lists of exprs
    feStmtListExpr    # xxx: do we really need this?
    feBlockExpr       # xxx: do we really need this?
    # TODO: not sure what to call this/where to put it
    feVarTuple
    # iterators
    feIteratorDef
    feYield
    feExpandIterator
    feFor
    # fancier templates
    feBind
    feBindStmt        # xxx: do we really need this?
    feMixin
    # procedural values
    feProcTy
    feLambda
    feDo
    feIteratorTy
    # effects
    feFuncDef
    feMutableTy       # xxx: parser support, but we never use it
    # comptime
    feStatic          ## static stmt or stmt list
    # type coerscion and branding
    feConverterDef
    feDistinctTy
    feWith
    feWithout
    # types - object oriented 
    feOfInherit       # xxx: move earlier for "embedding"?
    feMethodDef
    # generics
    feInstGeneric
    feGenericParams
    feRecWhen
    # type "classes" # xxx: figure out a better name for these
    feTupleClassTy
    # types - existentials
    feConceptTy       ## `nkTypeClassTy` for dumb legacy reasons
    feArgList
    # term-rewriting
    feUsingStmt
    # macros
    feMacroDef
    feEvalMacro
    # xxx: `feDiagnostic` isn't fully thought through, need to consider error
    #      recovery

  FirstOutstruction* = object
    status*: OutstrStatus
    case evt*: FirstEvtKind:
      of feCompile:
        newRunId*: RunId
      of feModule, feImport:
        moduleId*: ModuleId
      of feComment:
        commentId*: CommentId
      else:
        discard # TODO: write the rest of these branches!

  FirstInterpreterRunState = object
    ## state pertaining specifically to the run as opposed to more runtime
    ## module-esque dependencies.
    baseRunId: RunId  ## initialize this to -2 to indicate never set, otherwise
                      ## when compared to another runId:
                      ## * if this is lower, then this is a parent run
                      ## * if this is equal, then it's the same run
                      ## * if less than, then we're a child run or unrelated
    runSerial: int32  ## how we generate runIds

  FirstInterpreter* = object
    ## opaque type representing the interpreter, used to close over necessary
    ## dependencies and state such that everything else can use it in blissful
    ## ignorace.
    # xxx: wrt naming: this might be the module or semantic module interpreter?
    # legacyGraph: ModuleGraph
    logger: InterpreterLogger
    runState: FirstInterpreterRunState

# logger interface/impl start

proc startEvt*(logger: var InterpreterLogger, run: RunId, phase: PhaseId,
               tag: UntypedEvtTag, data: uint64) =
  logger.data.add LogEntry(run: run, phase: phase, evtStatus: started,
                           evtTag: tag, data: data)

proc closeEvt*(logger: var InterpreterLogger, run: RunId, phase: PhaseId,
               status: range[successful..aborted], tag: UntypedEvtTag,
               data: uint64) =
  logger.data.add LogEntry(run: run, phase: phase, evtStatus: status,
                           evtTag: tag, data: data)

# logger interface/impl end

const
  runNotInitialized = RunId -2
  neverRanBefore* = RunId -1

func initInterpreter*(#[m: ModuleGraph]#): FirstInterpreter =
  FirstInterpreter(
    # legacyGraph: ModuleGraph,
    logger: InterpreterLogger(), # xxx: pass it in as an arg, maybe?
    runState: FirstInterpreterRunState(baseRunId: runNotInitialized))

proc legacyStartCompile*(interp: var FirstInterpreter, runId = neverRanBefore) =
  ## legacy integration - need to mark the log with compile start
  if runId.int32 == neverRanBefore.int32:
    interp.runState.baseRunId = RunId 0
    inc interp.runState.runSerial
  elif runId.int32 >= 0:
    interp.runState.baseRunId = RunId interp.runState.runSerial
    inc interp.runState.runSerial
  else:
    unreachable("invalid run id: " & $runId.int32)

  # TODO: abstract this data writing
  let data = (uint64(interp.runState.baseRunId.int32) shl 32) or
             uint64(runId.int32)

  interp.logger.startEvt(interp.runState.baseRunId, phaseFirst,
                         feCompile.UntypedEvtTag, data)

proc legacyFinishCompile*(interp: var FirstInterpreter, runId: RunId) =
  assert interp.runState.baseRunId.int32 == runId.int32
  interp.logger.closeEvt(runId, phaseFirst, successful,
                         feCompile.UntypedEvtTag, 0)
  # TODO: should handle the "commit" here... ugh, need to tie into errors, man
  #       I hope that doesn't include too much legacy reports stupidity.

proc legacyProcessModule*(interp: var FirstInterpreter, moduleId: ModuleId) =
  interp.logger.startEvt(interp.runState.baseRunId, phaseFirst,
                         feModule.UntypedEvtTag, moduleId.uint64)

proc legacyProcessSystemModule*(interp: var FirstInterpreter, moduleId: ModuleId) =
  ## legacy integration - because the system module owns core sym/type
  ## definitions we need special handling
  # TODO: write whatever extra code to handle this terrible approach to the
  #       system module
  interp.logger.startEvt(interp.runState.baseRunId, phaseFirst,
                         feModule.UntypedEvtTag, moduleId.uint64)

proc legacyFinishModule*(interp: var FirstInterpreter, moduleId: ModuleId) =
  ## legacy integration - to finish processing a module
  # TODO: write whatever extra code to handle errors etc
  interp.logger.closeEvt(interp.runState.baseRunId, phaseFirst, successful,
                         feModule.UntypedEvtTag, moduleId.uint64)

from compiler/ast/ast_types import PNode, TNodeKind, nodeKindsProducedByParse

proc legacyProcessModuleStmt*(interp: var FirstInterpreter, moduleId: ModuleId, s: PNode) =
  ## legacy integration - mostly so we can auto-import the system module in
  ## `sem.semStmtAndGenerateGenerics`
  # xxx: should this be an "feAutoImport" instead so we can differentiate it,
  #      do we save some data to track it, or do we ignore it all together?
  var
    nodeStack: seq[PNode]
    syn: SyntaxDatum
    n = s
  while n != nil:
    case n.kind
    of nkEmpty: syn.tree.add(SyntaxDatumNode(kind: synEmpty))
    of nkIdent:
      syn.tree.add(SyntaxDatumNode(kind: synIdent, ident: n.ident))
    of nkCharLit: syn.tree.add(SyntaxDatumNode(kind: synCharLit, charLit: n.intVal.char))
    of nkIntLit: syn.tree.add(SyntaxDatumNode(kind: synIntLit, intLit: n.intVal.BiggestInt))
    of nkInt8Lit: syn.tree.add(SyntaxDatumNode(kind: synInt8Lit, int8Lit: n.intVal.int8))
    of nkInt16Lit: syn.tree.add(SyntaxDatumNode(kind: synInt16Lit, int16Lit: n.intVal.int16))
    of nkInt32Lit: syn.tree.add(SyntaxDatumNode(kind: synInt32Lit, int32Lit: n.intVal.int32))
    of nkInt64Lit: syn.tree.add(SyntaxDatumNode(kind: synInt64Lit, int64Lit: n.intVal.int64))
    of nkUIntLit: syn.tree.add(SyntaxDatumNode(kind: synUIntLit, uintLit: n.intVal.BiggestUInt))
    of nkUInt8Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt8Lit, uint8Lit: n.intVal.uint8))
    of nkUInt16Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt16Lit, uint16Lit: n.intVal.uint16))
    of nkUInt32Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt32Lit, uint32Lit: n.intVal.uint32))
    of nkUInt64Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt64Lit, uint64Lit: n.intVal.uint64))
    of nkFloatLit: syn.tree.add(SyntaxDatumNode(kind: synFloatLit, floatLit: n.floatVal.BiggestFloat))
    of nkFloat32Lit: syn.tree.add(SyntaxDatumNode(kind: synFloat32Lit, float32Lit: n.floatVal.float32))
    of nkFloat64Lit: syn.tree.add(SyntaxDatumNode(kind: synFloat64Lit, float64Lit: n.floatVal.float64))
    of nkFloat128Lit: syn.tree.add(SyntaxDatumNode(kind: synFloat128Lit, float128Lit: n.floatVal.BiggestFloat))
    of nkStrLit: syn.tree.add(SyntaxDatumNode(kind: synStrLit,
                                              strLit: (let idx = syn.strs.len; syn.strs.add n.strVal; StrIdx idx)))
    of nkRStrLit: syn.tree.add(SyntaxDatumNode(kind: synRStrLit,
                                              strLit: (let idx = syn.strs.len; syn.strs.add n.strVal; StrIdx idx)))
    of nkTripleStrLit: syn.tree.add(SyntaxDatumNode(kind: synTripleStrLit,
                                              strLit: (let idx = syn.strs.len; syn.strs.add n.strVal; StrIdx idx)))
    of nkNilLit: syn.tree.add(SyntaxDatumNode(kind: synNilLit))
    of nkCustomLit: synCustomLit
    of nkAccQuoted: synAccQuoted
    of nkCall: synCall
    of nkCommand: synCommand
    of nkCallStrLit: synCallStrLit
    of nkInfix: synInfix
    of nkPrefix: synPrefix
    of nkPostfix: synPostfix
    of nkExprEqExpr: synExprEqExpr
    of nkExprColonExpr: synExprColonExpr
    of nkIdentDefs: synIdentDefs
    of nkConstDef: synConstDef
    of nkVarTuple: synVarTuple
    of nkPar: synPar
    of nkSqrBracket: synSqrBracket
    of nkCurly: synCurly
    of nkTupleConstr: synTupleConstr
    of nkObjConstr: synObjConstr
    of nkTableConstr: synTableConstr
    of nkSqrBracketExpr: synSqrBracketExpr
    of nkCurlyExpr: synCurlyExpr
    of nkPragmaExpr: synPragmaExpr
    of nkPragma: synPragma
    of nkPragmaBlock: synPragmaBlock
    of nkDotExpr: synDotExpr
    of nkIfExpr: synIfExpr
    of nkIfStmt: synIfStmt
    of nkElifBranch: synElifBranch
    of nkElifExpr: synElifExpr
    of nkElse: synElse
    of nkElseExpr: synElseExpr
    of nkCaseStmt: synCaseStmt
    of nkOfBranch: synOfBranch
    of nkWhenExpr: synWhenExpr
    of nkWhenStmt: synWhenStmt
    of nkForStmt: synForStmt
    of nkWhileStmt: synWhileStmt
    of nkBlockExpr: synBlockExpr
    of nkBlockStmt: synBlockStmt
    of nkDiscardStmt: synDiscardStmt
    of nkContinueStmt: synContinueStmt
    of nkBreakStmt: synBreakStmt
    of nkReturnStmt: synReturnStmt
    of nkRaiseStmt: synRaiseStmt
    of nkYieldStmt: synYieldStmt
    of nkTryStmt: synTryStmt
    of nkExceptBranch: synExceptBranch
    of nkFinally: synFinally
    of nkDefer: synDefer
    of nkLambda: synLambda
    of nkDo: synDo
    of nkBind: synBind
    of nkBindStmt: synBindStmt
    of nkMixinStmt: synMixinStmt
    of nkCast: synCast
    of nkStaticStmt: synStaticStmt
    of nkAsgn: synAsgn
    of nkGenericParams: synGenericParams
    of nkFormalParams: synFormalParams
    of nkStmtList: synStmtList
    of nkStmtListExpr: synStmtListExpr
    of nkImportStmt: synImportStmt
    of nkImportExceptStmt: synImportExceptStmt
    of nkFromStmt: synFromStmt
    of nkIncludeStmt: synIncludeStmt
    of nkExportStmt: synExportStmt
    of nkExportExceptStmt: synExportExceptStmt
    of nkConstSection: synConstSection
    of nkLetSection: synLetSection
    of nkVarSection: synVarSection
    of nkProcDef: synProcDef
    of nkFuncDef: synFuncDef
    of nkMethodDef: synMethodDef
    of nkConverterDef: synConverterDef
    of nkIteratorDef: synIteratorDef
    of nkMacroDef: synMacroDef
    of nkTemplateDef: synTemplateDef
    of nkTypeSection: synTypeSection
    of nkTypeDef: synTypeDef
    of nkEnumTy: synEnumTy
    of nkEnumFieldDef: synEnumFieldDef
    of nkObjectTy: synObjectTy
    of nkTupleTy: synTupleTy
    of nkProcTy: synProcTy
    of nkIteratorTy: synIteratorTy
    of nkRecList: synRecList
    of nkRecCase: synRecCase
    of nkRecWhen: synRecWhen
    of nkTypeOfExpr: synTypeOfExpr
    of nkRefTy: synRefTy
    of nkVarTy: synVarTy
    of nkPtrTy: synPtrTy
    of nkStaticTy: synStaticTy
    of nkDistinctTy: synDistinctTy
    of nkMutableTy: synMutableTy
    of nkTupleClassTy: synTupleClassTy
    of nkTypeClassTy: synTypeClassTy
    of nkOfInherit: synOfInherit
    of nkArgList: synArgList
    of nkWith: synWith
    of nkWithout: synWithout
    of nkAsmStmt: synAsmStmt
    of nkCommentStmt: synCommentStmt
    of nkUsingStmt: synUsingStmt
    of {low(TNodeKind) .. high(TNodeKind)} - nodeKindsProducedByParse:
      discard
