## Not really a "new AST", just not sure what to call it yet
## 
# INTERPRETER-AS-COMPILER STARTS HERE
# TODO:
# 1. generate in-to-out-structions: `nim --compilepreter c foo.nim`
#   - [ ] rig up compilepreter/package-lang into ConfigRef (set base RunId)
#   - [X] rig up module-lang interpreter into ModuleGraph
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
#
# Architecture Sketch: stack of interpreters that extend language capabilities:
# - compilepreter: runs/ochestrates everything below
# - packages: supporrt multi-module + package concept
# - modules: support modules/multi-file programs
# - module: single file programs (might merge with modules)
# - 

import compiler/compilepreter/interpreterlogger
from compiler/front/options import ConfigRef
from compiler/ast/idents import IdentCache, PIdent
from compiler/ast/lineinfos import FileIndex

export RunId # xxx: ugh, export, find a way to remove it

# import modulegraphs, because it seems `FirstInterpreter` layers over it...
# still figuring out how this should work.
from compiler/utils/idioms import unreachable
# from compiler/modules/modulegraphs import ModuleGraph
  # TODO: this is a recursive dependency, move "FirstInterpreter" out of this module

type
  ## Some types start with "First", because they're the first interpeter we
  ## started defining, and to avoid getting stuck on names from the outset.

  ModuleId* = distinct int32
  PkgId* = distinct int32

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
    legacyConfig: ConfigRef
    identCache: IdentCache
    logger: InterpreterLogger
    runState: FirstInterpreterRunState

const
  runNotInitialized = RunId -2
  neverRanBefore = RunId -1

func initInterpreter*(config: ConfigRef, cache: IdentCache): FirstInterpreter =
  FirstInterpreter(
    legacyConfig: config,
    identCache: cache,
    logger: InterpreterLogger(),
    runState: FirstInterpreterRunState(baseRunId: runNotInitialized)
  )

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

proc legacyDiscoverPkg*(interp: var FirstInterpreter, ident: PIdent): PkgId =
  ## 
  let maybeId = interp.runState.pkgs.find(ident)
  if maybeId == -1:
    result = PkgId interp.runState.pkgs.len.int32
    interp.runState.pkgs.add(ident)
  else:
    result = PkgId maybeId.int32

proc legacyStartModule*(interp: var FirstInterpreter, ident: PIdent,
                        fileIdx: FileIndex, pkgId: PkgId) =
  let data = (uint64(pkgId.int32) shl 32) or uint64(fileIdx.int32)
  interp.logger.startEvt(interp.runState.baseRunId, phaseFirst,
                         feModule.UntypedEvtTag, data)