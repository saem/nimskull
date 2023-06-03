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
  synUptoUnaryKinds = {synDiscard, synReturn, synContinue, synBreak, synRaise,
                       synRefTy, synVarTy, synPtrTy}
    # xxx: split these into dedicated kinds leaf vs unary
  synUnaryKinds = {synCallStrLit, synYield, synBind, synStaticStmt, synMutableTy}
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
                   synConstSection, synLetSection, synVarSection,
                   synTypeSection, synTypeDef, synEnumTy, synObjectTy,
                   synTupleTy, synProcTy, synIteratorTy, synRecList,
                   synRecCase, synRecWhen, synTypeOfExpr, synStaticTy,
                   synDistinctTy, synTypeClassTy, synOfInherit, synArgList,
                   synWith, synWithout, synAstStmt, synUsingStmt}
    ## syntax nodes with 'n' kids (variable width)
    ## xxx: this category can likely be broken down further after fixing the
    ##      rest of the compiler and refining the structures

type
  IdentIdx = distinct int32
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
      of synCustomLit:      litPart: StrIdx, suffix: IdentIdx
      of synUptoUnaryKinds: unary: bool ## if true next node is child
      of synLeafKinds:      discard
      of synUnaryKinds:     discard ## next node is the child
      of synBinaryKinds:    rightId: TreeIdx # left always follows the parent
      of synRoutineDefKinds:extraId: ExtraIdx
      of synNKidsKinds:     extraId: ExtraIdx, count: Natural

  SyntaxDatum = object
    ## an abstract syntax tree as a singular unit of data (datum).
    tree: seq[SyntaxDatumNode]
    extra: seq[TreeIdx]
    ident: seq[string]
    strs: seq[string]

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
#   - sketch out `FirstInstruction` and `FirstOutstruction`
#   - rig up the `interpret` proc in `sem`
#   - just start seeing it echo
#   - revise goals
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

type
  ## various identifiers to point at various pieces of data
  # xxx: need to incorporate once we get to environments
  RundId* = distinct int32
  PhaseId* = distinct int32
  ModuleId* = distinct int32

type
  OutstrStatus* {.pure.} = enum
    started
    successful ## finished successfullly
    failed     ## finished, with errors
    cancelled  ## stopped by the interpreter
    aborted    ## stopped by the caller

type
  InstructionSource* = object
  OutstructionTarget* = object

type
  ## Some types start with "First", because they're the first interpeter we
  ## started defining, and to avoid getting stuck on names from the outset.

  FirstInstrKind* {.pure.} = enum
    startCompile      ## tie-in with `sem.semPass` and `main` initialization
    processModule     ## tie-in with `sem.myOpen`
    processStatement  ## tie-in with `sem.myProcess/semStmtAndGenerateGenerics`
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

  FirstInstruction* = object
    case op*: FirstInstrKind:
      of startCompile:     currRunId*: RunId
      of processModule:    moduleId*: ModuleId
      of processStatement: discard # xxx: how to refer to the AST?
      of importModule:     moduleId*: ModuleId
      of finishCompile:    discard
      of abortCompile:     discard

  FirstOutstruction* = object
    status*: OutstrStatus
    case evt*: FirstEvtKind:
      of feCompile:
        newRunId*: RunId
      of feModule, feImport:
        moduleId*: ModuleId
      of feComment:
        commentId*: 
      # TODO: write the rest of these branches!

  FirstInterpreter* = object

func initInterpreter*(): FirstInterpreter =
  discard

func interpret*(interp: FirstInterpreter, instr: FirstInstruction): FirstOutstruction =
  discard "rig me up already!"