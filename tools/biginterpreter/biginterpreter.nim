import std/unittest

import std/streams
import experimental/sexp
import std/strutils

# `cyo [run] foo.cyo` executes the cyo program in foo.cyo, the `[run]` part is
# optional
#
# A "big idea" with cyo is that everything immediately runs, there is no
# compile to an exe. If one wants to compile to an exe they write a program
# that emits a "program value". In essence they write a build program and the
# compiler is treated as a library providing a standard build CLI interface.
# This interface should be used or enjoy making an ecosystem. Now invoking the
# "compiler" is akin to calling a function via the CLI that in turn calls a
# build function. The build dependencies are reduced to which compiler exe,
# build program, and the dependencies of the build program itself. Program
# dependencies (packages, resources, etc) are now the builds' problem, which
# may use the provided standard approach or do something else. The language
# understands packages, their versions and so on, but the implementation of
# operations (lookup, resolution, etc) are free. The stdlib ships a "CLI
# protocol" and default implementation(s) covering things like debug, build,
# testing, CI, IDEs, docs, linting, etc and people can choose to use it or not.
#
# misc notes
# ==========
#
# dispatch priority
# -----------------
# - untyped params have highest match priority in dispatch
# - then specific types
# - then generic types
# - then conversion
# - then typed
#
#
type
  CyoTokenKind = enum
    ## tokens... finish implementing
    cytkError  ## boo an error
  
  CyoSourceTree = enum
    ## concrete syntax, touch more than raw tokens, for pure source transforms
    ## finish implementing, gets converted into `CyoAbstractSyntaxKind`
    cstkError ## boo an error

  CyoAbstractSyntaxKind = enum
    ## abstract syntax tree comes out of the parser after some normalization,
    ## gets converted into `CyoCoreRepresentationKind`
    caskError ## boo an error
    
    caskMetaHeader ## top of a source file metadata which cyo version and
                   ## stdlib eg: `##!(:cyo (:version 1) (:stdlib cyo std))`
                   ## starts with `##!` and the rest of the content is a sexp
                   ## xxx: sexps are cool... or be consistent with pragmas?

    caskEmpty      ## an empty node... specializing nodes removes the need

    caskComment ## single line comment `#` or `#[]#` comment range
    caskDoc     ## single line documentation comment `##` or `##[]##` range,
                ## associates to the AST node containing this group

    # Literals - Start
    caskLitInt       ## integer literal, `1`, `-9`
    caskLitInt8      ## integer 8 bit literal, `1'i8`, `-43'i8`
    caskLitInt16     ## integer 16 bit literal, `1'i16`, `-43'i16`
    caskLitInt32     ## integer 32 bit literal, `1'i32`, `-43'i32`
    caskLitInt64     ## integer 64 bit literal, `1'i64`, `-43'i64`
    
    caskLitUInt      ## unsigned integer literal, `0'u`, `95'u`
    caskLitUInt8     ## unsigned integer 8 bit literal, `1'u8`, `52'u8`
    caskLitUInt16    ## unsigned integer 16 bit literal, `1'u16`, `52'u16`
    caskLitUInt32    ## unsigned integer 32 bit literal, `1'u32`, `52'u32`
    caskLitUInt64    ## unsigned integer 64 bit literal, `1'u64`, `52'u64`
    
    caskLitFloat     ## float literal, `1.0`
    caskLitFloat32   ## float 32 bit literal, `1.0f32`, `0'f`
    caskLitFloat64   ## float 64 bit literal, `1.0f64`, `0'd`
    caskLitFloat128  ## float 128 bit literal

    caskLitChar      ## character literal, 8 bit, `'a'`, `'\n'`, `'\69'`

    caskLitNil       ## nil literal

    caskLitStr       ## string literal
    caskLitTripleStr ## string triple quoted literal
    # xxx are "raw" string literals required?
    # Literals - End
    
    caskIdent     ## identifier
    caskAccQuoted ## eg: `var`, unkeywords things

    caskDotExpr
    caskCall
    caskCommand
    # xxx: are "generalized" string literals required?

    caskInfix   ## a binary infix operation, eg: `1 / 2` or `cyo/std`
    caskPostfix ## postfix operation, eg: `foo*`
    caskPrefix  ## prefix operation

    caskPragma    ## instructions to the compiler, eg: `{.foo.}`
    caskPragmaSpan ## `{.foo.}: ...`, where `...` are expressions or statements
    # xxx: skipping pragma blocks, don't think they make sense 

    caskPar
    caskBracket
    caskBracketExpr
    caskBrace
    caskBraceExpr

    caskTupleConstr ## 
    caskObjConstr   ## 
    caskTableConstr ## `{a: 1, b: 2}`

    caskExprEqExpr
    caskExprColonExpr

    caskBlock      ## block
    caskBlockLabel ## labelled block

    caskIf
    caskElif       ## xxx: can we replace with with `else if ...:`?
    caskElse
    caskCase
    caskOf
    caskWhen

    caskWhile
    caskFor

    caskContinue      ## continue without a label
    caskContinueLabel ## continue with a label
    caskBreak         ## break without a label
    caskBreakLabel    ## break with a label

    caskReturn        ## return without an expression
    caskReturnExpr    ## return with an expression on the right

    caskDiscard
    caskDiscardExpr

    caskYield
    caskYieldExpr

    caskTry
    caskExcept
    caskFinally

    caskRaise

    caskDefer

    caskConstSection
    caskLetSection
    caskVarSection

    caskIdentDefs
    # nkConstdef
    caskUnpackDef

    caskAsgn

    caskLambda
    # nkDo
    
    caskStaticSpan  ## static span
    caskCast

    caskProcDef    ## procedure definition
    caskFuncDef    ## function definition
    caskMethodDef
    caskConverterDef
    caskIteratorDef
    # xxx: thinking of not doing macros or templates, just have quoting and
    #      macros and templates are just procedures and functions that take
    #      typed or untyped arguments

    caskQuote ## quoted syntax between `< ... >` xxx: hmmm

    caskTemplateDef ## procedure with 1 or more `untyped|typed` params and a
                    ## return type of `untyped` as the signature
    caskMacroDef ## prefix to a func or proc definition, allowing it to accept function with 1 or more `untyped|typed` params and a
                     ## return type of `untyped{noeffects}` as the signature

    caskEval ## identifier prefixed with an `@`, eg: `@cps ...` passes all
             ## untyped syntax right of the identifier to a macros `cps`



  CyoCoreRepresentationKind = enum
    ## the internal lisp-y core, this is before we drop to codegen land
    ccrkError ## boo an error






# =====================================
# IGNORE EVERYTHING BELOW HERE
# =====================================


# ------------------------------------------------------------------------
# playing with cyo ideas but using sexp to avoid faffing about with syntax
# ------------------------------------------------------------------------

proc parse(parser: var SexpParser, src: string): (SexpNode, ref SexpParsingError) =
  ## simple sexp parser for testing
  parser.open(newStringStream(src))
  discard getTok(parser)    # read the first token; "prime" the parser
  result = 
    try:
      (parser.parseSexp(), nil)
    except SexpParsingError as e:
      (nil, e)
    finally:
      parser.close()


let srcHelloWorld = """
(
  (:cyo (:version (0 1)) (:stdlib (cyo std)))

  (:open std)

  (:let greeting "Hello")
  (:var target "World")

  (echo greeting ", " target "!")
)
""".strip()

let srcblah = """
(
  (:cyo (:version (0 1)))

  (:types
    (:type Subrange (range 0 5))
    (:type PositiveFloat (range 0.0 Inf))
  )
)
""".strip()

suite "whatever":
  test "hello world":
    var sexpParser: SexpParser
    let (parsed, err) = sexpParser.parse(srcHelloWorld)

    echo srcHelloWorld, "\n", parsed

  test "blah":
    # TODO finish writing this test
    var sexpParser: SexpParser
    let (parsed, err) = sexpParser.parse(srcblah)

    echo srcblah, "\n", parsed

# Ignore stuff below this

# proj(somename, #[code]# empty)
# source_text | source_data -> project
# analyse: analyser -> host -> env -> project_desc -> project
# compile: compiler -> host -> env -> partial[input] -> project -> result[interpretable]
# compile: compiler -> target -> partial[input] -> interpretable -> result[executable]
# interpret: interpreter -> host -> env -> partial[input] -> interpretable -> partial[input] -> result[output|interpretable]

# type # useful general things
#   Version = object
#     major: int
#     minor: int

#   Source = string

# type # project description  
#   ProjDescNodeKind {.pure.} = enum
#     pdnMeta
#     pdnModule

#   ProjDescNode = object
#     case kind: ProjDescNodeKind
#     of pdnMeta:
#       version: string
#     of pdnModule:
#       discard

#   ProjectDesc = object
#     data: seq[ProjDescNode]

# type # top level data
#   Analyser = object
#     discard
#   Compiler = object
#     analyser: Analyser
#   Interpreter = object
#     discard
  
#   Project = object
#     version: Version
  
#   HostInf = object
#     discard
#   HostData = object
#     discard

#   EnvData = object
#     discard

#   Interpretable = object
#     discard

#   Target = object
#     discard

#   InputDesc = object
#     discard

#   ExecutableDesc = object
#     discard

#   ExecutionDesc = object
#     stdOut: string
#     stdErr: string
#     exitCode: int

# proc analyse(a: Analyser, h: HostInf, e: EnvData, d: ProjectDesc): Project =
#   var version: string
#   for n in d.data:
#     case n.kind
#     of pdnMeta:
#       version = n.version
#     of pdnModule:
#       discard

#   Project(version: Version version)

# proc compile(c: Compiler, h: HostInf, e: EnvData, d: ProjectDesc, i: InputDesc): Interpretable =
#   let
#     project = analyse(c.analyser, h, e, d)
#     assume = """
# (:cyo (:version (0 1)) (:std cyo))
# (:require (std system))

# """

#   Interpretable()

# proc interpret(i: Interpreter, h: HostInf, e: EnvData, p: Interpretable, input: InputDesc): ExecutionDesc =
#   ExecutionDesc(stdOut: "Hello, World!", stdErr: "", exitCode: 0)

# suite "language as a function":
#   test "analyse":
#     let
#       givenProjDesc =
#         ProjectDesc(
#           data: @[
#             ProjDescNode(kind: pdnMeta, version: "0.0.1")
#           ]
#         )
#       actual = analyse(Analyser(), HostInf(), EnvData(), givenProjDesc)
    
#     check(actual == Project(version: Version "0.0.1"))
  
#   test "compile":
#     let
#       givenProjDesc =
#         ProjectDesc(
#           data: @[
#             ProjDescNode(kind: pdnMeta, version: "0.0.1")
#           ]
#         )
#       actual = compile(Compiler(), HostInf(), EnvData(), givenProjDesc, InputDesc())
    
#     check(actual == Interpretable())

#   test "interpret":
#     let
#       givenInterpretable =
#         block:
#           let
#             compiler = Compiler()
#             host = HostInf()
#             env = EnvData()
#             projDesc =
#               ProjectDesc(
#                 data: @[
#                   ProjDescNode(kind: pdnMeta, version: "0.0.1")
#                 ]
#               )
#             input = InputDesc()
#           compile(compiler, host, env, projDesc, input)      
#       actual = interpret(Interpreter(), HostInf(), EnvData(), givenInterpretable, InputDesc())
      
#     check(actual == ExecutionDesc(stdOut: "Hello, World!"))

# type
#   Interpreter = object
#     state: InterpreterState
#     env: Environments

#   Source = string

#   Environments = object
#     supportedVersions: seq[Version]
#     global: EnvironmentData

#   EnvironmentData = object
#     discard
#     # kind: EnvironmentKind
#     # of parentId: EnvironmentId
  
#   # EnvironmentId = distinct int

#   Interpretation = object
#     kind: InterpretationKind

#   InterpretationKind {.pure.} = enum
#     voidProgram
#     invalidProgram
#     validProgram

#   InterpreterState {.pure.} = enum
#     intrpNotStarted
#     intrpMeta
#     intrpEval
#     intrpError
#     intrpOutput
#     intrpDone
  
#   Version = distinct string

# proc createInterpreter(): Interpreter =
#   Interpreter(
#     env: Environments(
#       supportedVersions: @[Version "0.0.1"]
#       )
#     )

# type
#   EvaluationResult = object
#     kind: EvaluationResultKind
#     # srcId: SourceId # TODO: make a distinct for this
#     # left, right: SourceSeek # TODO: make a distinct for this
  
#   EvaluationResultKind {.pure.} = enum
#     evaluationEmptyList
#     evaluationApplicationStart
#     evaluationApplicationEnd
#     evaluationLiteralString
#     evaluationLiteralInt
#     evaluationLiteralFloat

# proc evaluation(i: var Interpreter, sexp: SexpNode): EvaluationResult =
#   # TODO: validate the sexp based on interpreter phase
#   case sexp.kind
#   of SList:
#     case sexp.len
#     of 0:
#       result = EvaluationResult(kind: evaluationEmptyList)
#     else:
#       case sexp[0].kind
#       of SSymbol:
#         let symRes = i.env.symbolQuery(sexp[0])
#         # if legit set for apply, else error handling
#       of SNil, SInt, SFloat, SString, SKeyword, SList, SCons:

#     for s in sexp:
#       case s.kind
#       of SSymbol:
#         let symRes = i.env.symbolQuery(s)
#         case symRes.kind
#         of symFound:
#           for a in s
#       of SNil, SInt, SFloat, SString, SKeyword, SList, SCons:
#         result = Interpretation(kind: invalidProgram)

# proc interpret(i: var Interpreter, src: Source): Interpretation =
#   var sexpParser: SexpParser
#   let (parsed, err) = sexpParser.parse(src)

#   if err.isNil:
#     for s in parsed.items: # relies on it being a list
#       case i.state
#       of intrpNotStarted:
#         case s.kind
#         of SSymbol:
#           i.state = intrpEval
#         of SNil, SInt, SFloat, SString, SKeyword, SList, SCons:
#           i.state = intrpError
#       of intrpMeta:
#         # interpreter.metaEvaluation(s)
#         result = Interpretation(kind: voidProgram)
#       of intrpEval:
#         discard
#       of intrpError:
#         result = Interpretation(kind: invalidProgram)
#         i.state = intrpDone
#       of intrpOutput:
#         discard
#       of intrpDone:
#         discard
#   else:
#     i.state = intrpError
#     result = Interpretation(kind: invalidProgram)

# suite "Interpret programs":

#   # Every program has at least one module, called the init module.

#   test "empty programs are fine":
#     let emptyPrograms = [
#         """(meta (:version "0.0.1"))""",
#         "(meta (:version \"0.0.1\"))  \n  ",
#       ]
#     for s in emptyPrograms:
#       var i: Interpreter = createInterpreter()
#       let o = interpret(i, s)
#       check(o.kind == voidProgram)

#   test "meta with version must be on the first line":
#     let invalidPrograms = ["\n(meta (:version \"0.0.1\"))", ""]
#     for p in invalidPrograms:
#       var i: Interpreter = createInterpreter()
#       check(interpret(i, p).kind == invalidProgram)

#   test "invalid versions result in errors":
#     let invalidVersion = "(meta (:version \"0.0.3\"))"
#     var i: Interpreter = createInterpreter()
#     check(interpret(i, invalidVersion).kind == invalidProgram)

#   test "Hello, World!":
#     let helloProg = """(meta (:version "0.0.1"))
#     (echo "Hello, World!")
#     """
#     # var p: SexpParser
#     # let (s, e) = p.parse(helloProg)
#     # echo s
#     # var i: Interpreter = createInterpreter()
#     # check(interpret(i, helloProg).kind == validProgram)
#   #   check(interpret("", initModule).kind == )