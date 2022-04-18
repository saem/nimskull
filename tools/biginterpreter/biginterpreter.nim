import std/unittest

import std/streams
import experimental/sexp

type
  Interpreter = object
    state: InterpreterState
    env: Environment

  Source = string

  Environment = object
    discard

  Interpretation = object
    kind: InterpretationKind

  InterpretationKind {.pure.} = enum
    voidProgram
    invalidProgram
  
  InterpreterState {.pure.} = enum
    intrpNotStarted
    intrpMeta
    intrpEval
    intrpError
    intrpOutput
    intrpDone

proc interpret(i: var Interpreter, src: Source): Interpretation =
  var sexpParser: SexpParser
  sexpParser.open(newStringStream(src))
  discard getTok(sexpParser)    # read the first token; "prime" the parser
  let (parsed, err) =
    try:
      (sexpParser.parseSexp(), nil)
    except SexpParsingError as e:
      (nil, e)
    finally:
      sexpParser.close()

  if err.isNil:
    for s in parsed.items: # relies on it being a list
      case i.state
      of intrpNotStarted:
        case s.kind
        of SKeyword:
          i.state = intrpMeta
        of SNil, SInt, SFloat, SString, SSymbol, SList, SCons:
          i.state = intrpError
      of intrpMeta:
        result = Interpretation(kind: voidProgram)
      of intrpEval:
        discard
      of intrpError:
        result = Interpretation(kind: invalidProgram)
        i.state = intrpDone
      of intrpOutput:
        discard
      of intrpDone:
        discard
  else:
    i.state = intrpError
    result = Interpretation(kind: invalidProgram)


suite "Interpret programs":

  # Every program has at least one module, called the init module.

  test "empty programs are fine":
    let emptyPrograms = [
        """(meta (:version "0.0.1"))""",
        "(meta (:version \"0.0.1\"))  \n  ",
      ]
    for s in emptyPrograms:
      var i: Interpreter
      let o = interpret(i, s)
      check(o.kind == voidProgram)

  test "meta and version must be on the first line":
    let invalidPrograms = ["\n(meta (:version \"0.0.1\"))", ""]
    for p in invalidPrograms:
      var i: Interpreter
      check(interpret(i, p).kind == invalidProgram)

  # test "Hello, World!":
  #   let initModule = """(meta {:version "0.0.1", :name myProgram})"""
  #   check(interpret("", initModule).kind == )