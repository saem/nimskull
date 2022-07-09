import std/unittest

import std/streams
import experimental/sexp
import std/strutils

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

# proj(somename, #[code]# empty)

# source_text | source_data -> project
# analyse: analyser -> host -> env -> project_desc -> project
# compile: compiler -> host -> env -> partial[input] -> project -> result[interpretable]
# compile: compiler -> target -> partial[input] -> interpretable -> result[executable]
# interpret: interpreter -> host -> env -> partial[input] -> interpretable -> partial[input] -> result[output|interpretable]

# `cyo [run] foo.cyo` executes the cyo program in foo.cyo, the `[run]` part is optional

type
  CyoExe = object
    discard

  CyoData = object
    cmd: CyoCmd
    args: string

  CyoRun = object
    source: string

  CyoCmd = enum
    cyoCmdRun

let foo = """
(
  (:cyo (:version (0 1)) (:stdlib (cyo std)))

  (:open std)

  (:let greeting "Hello")
  (:var target "World")

  (echo greeting ", " target "!")
)
""".strip()

suite "whatever":
  test "thing":
    var sexpParser: SexpParser
    let (parsed, err) = sexpParser.parse(foo)

    echo foo, "\n", parsed
    # echo parsed

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