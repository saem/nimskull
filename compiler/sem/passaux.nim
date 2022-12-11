#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## implements some little helper passes

import
  compiler/ast/[
    ast,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/sem/[
    passes
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportSem
from compiler/ast/report_enums import ReportKind

type
  VerboseRef = ref object of PPassContext
    config: ConfigRef

proc verboseOpen(graph: ModuleGraph; s: PSym; idgen: IdGenerator): PPassContext =
  # xxx consider either removing this or keeping for documentation for how to add a pass
  result = VerboseRef(config: graph.config, idgen: idgen)

proc verboseProcess(context: PPassContext, n: PNode): PNode =
  # called from `process` in `processTopLevelStmt`.
  result = n
  let v = VerboseRef(context)
  localReport(v.config, n, reportSem rsemProcessingStmt)

const verbosePass* = makePass(open = verboseOpen, process = verboseProcess)
