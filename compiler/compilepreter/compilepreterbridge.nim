## bridge into the legacy compiler, by way of the compiler passes mechanism.
## if the compilepreter works out, it'll basically eat almost all of it.

import
  compiler/compilepreter/[
    interpreterlogger,
    synobj,
  ],
  compiler/modules/modulegraphs

from compiler/ast/idents import IdGenerator
from compiler/ast/ast_types import PSym
from compiler/modules/passes import makePass, PPassContext, TPassContext

type
  ModuleInterpreter = object
    legacyGraph: ModuleGraph
    idgen: IdGenerator
    logger: InterpreterLoggerRef
    # TODO: capture base RunId for nested compilation

  TPassBridge = object of TPassContext
    interp: ModuleInterpreter
  PPassBridge = ref of TPassBridge

func initInterpreter*(m: ModuleGraph, idgen: IdGenerator): ModuleInterpreter =
  ModuleInterpreter(
    legacyGraph: ModuleGraph,
    idgen: idgen,
    logger: InterpreterLogger(),) # TODO, this is wrong, fetch a ref from args

proc startModule(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext =
  ## start processing a module, wire into the compiler passes system's open hook
  result = new(PPassBridge)
  result.interp = initInterpreter(graph, idgen)
  # tell the interperter to process the module, module.position for ModuleId:
  # - system module vs regular module
  when false: # old code to rrefer to
    interp.logger.startEvt(interp.runState.baseRunId, phaseFirst,
                          feModule.UntypedEvtTag, moduleId.uint64)

proc recvModuleStmt(p: PPassContext, topLevelStmt: PNode): PNode =
  ## receive top level stmt, or the entire module ast, wire into the compiler
  ## passes system's process hook.

  # TODO: convert AST and have interprerter process it

  result = topLevelStmt # identity proc, so the `sem` pass is unaffected

proc finishModule(graph: ModuleGraph; p: PPassContext, n: PNode): PNode =
  ## finish processing a module, wire into the compiler pass systems close hook
  let rid = RunId 0'i32  # TODO: use a real RunId
  PPassBridge(p).interp.logger.closeEvt(rid, phaseFirst,
                                        successful, feModule.UntypedEvtTag,
                                        moduleId.uint64)
  # TODO: write whatever code... ugh legacy reports, to determine the
  #       success/failure/etc behaviour

  result = n # identity proc, so the `sem` pass is unaffected

const compilepreterPass* = makePass(
  open = startModule,
  process = recvModuleStmt,
  close = finishModule,
  isFrontend = true
)