## bridge into the legacy compiler, by way of the compiler passes mechanism.
## if the compilepreter works out, it'll basically eat almost all of it.

import
  compiler/compilepreter/[
    interpreterlogger,
    synobj,
  ],
  compiler/modules/modulegraphs

from std/algorithms import reverse

from compiler/ast/idents import IdGenerator
from compiler/ast/ast_types import PSym
from compiler/modules/passes import makePass, PPassContext, TPassContext

type
  TPassBridge = object of TPassContext
    legacyGraph: ModuleGraph
    legacyModule: PSym
  PPassBridge = ref of TPassBridge

proc startModule(graph: ModuleGraph; module: PSym; idgen: IdGenerator): PPassContext =
  ## start processing a module, wire into the compiler passes system's open hook
  result = new(PPassBridge)
  result.legacyGraph = graph
  result.legacyModule = module

  # xxx: this isn't the "right" way to do package id, as the package id will be
  #      the same as the module that first led to its discovery (unstable)
  graph.compilepreter.legacyStartModule(ModuleId module.position, module.ident,
                                        module.info.fileIndex,
                                        module.owner.ident)

  when false: # old code to refer to
    interp.logger.startEvt(interp.runState.baseRunId, phaseFirst,
                          feModule.UntypedEvtTag, moduleId.uint64)

proc recvModuleStmt(p: PPassContext, topLevelStmt: PNode): PNode =
  ## receive top level stmt, or the entire module ast, wire into the compiler
  ## passes system's process hook.

  p.graph.compilepreter.legacyModuleStmt(ModuleId module.position,
                                         topLevelStmt)

  result = topLevelStmt # identity proc, so the `sem` pass is unaffected

proc finishModule(graph: ModuleGraph; p: PPassContext, n: PNode): PNode =
  ## finish processing a module, wire into the compiler pass systems close hook
  graph.compilepreter.legacyFinishModule(ModuleId module.position)

  result = n # identity proc, so the `sem` pass is unaffected

const compilepreterPass* = makePass(
  open = startModule,
  process = recvModuleStmt,
  close = finishModule,
  isFrontend = true
)