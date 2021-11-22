#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module is the new ast, it's an attempt to migrate the compiler towards
## a set of lowerings.
##
## Naming Conventions:
## * `legacy` related to bridging from old AST to the new one

# TODO:
# 1. tie into a special compiler command
# 2. run the old parser and graph to populate a compilation and the broader
#    interpretation of AST (meaning the whole project)
# 3. then start running through an interpreter

from ".." / ast import TNodeKind, PNode, safeLen, `[]`, pairs
import data, data_ast, data_token, data_module

type
  Compilation* = distinct ref CompilationData
    ## a particular compilation, distinct in order to provide a narrow
    ## itnerface over the compilation

  # ModuleAst* = distinct ModuleData

func ast(c: Compilation): var Ast = c.CompilationData.ast
func tokens(c: Compilation): var TokenList = c.CompilationData.tokens
func modules(c: Compilation): var seq[ModuleData] = c.CompilationData.modules

# func id*(m: ModuleAst): ModuleId = m.ModuleData.id
# func tokens*(m: var ModuleAst): var TokenList = m.ModuleData.tokens
# func ast*(m: var ModuleAst): var Ast = m.ModuleData.ast

# proc `$`*(m: ModuleAst): string {.inline.} =
#   $m.ModuleData

# proc initModuleAst*(id: ModuleId, hintAstLen = assumedAstLen): ModuleAst =
#   ## initialize a ModuleAst, pre-allocate storage based on the `hintAstLen` to
#   ## reduce excessive allocations/moves.
#   ModuleAst initModule(id, hintAstLen)

# proc legacyInitModuleAst*(id: int): ModuleAst =
#   ## creates `ModuleAst` with the legacy `id` of the module, in order to keep
#   ## it in sync with the legacy `ModuleGraph`.
#   initModuleAst(ModuleId id)

type
  NodeProcessingKind {.pure.} = enum
    ## xxx: the name leaves much to be desired -- to be renamed.
    ## The intention is to indicate further processing of child nodes based
    ## upon the kind. This further processing usually involves extra data, see:
    ## `Ast.extra` and `AstExtra`
    npkNoExtraData,
    npkIndexAllButFirstChild

proc legacyBeginModule*(c: Compilation; id: int) =
  # xxx: make this return a specialized `ModuleCompilation` type so we can only
  #      have a few sensible operations on it, still a `Compilation` underneath
  let
    mid = ModuleId id
    aidx = c.ast.reserveAstNode(ankModule, TokenIndex id)
    c.modules.add ModuleData(id: ModuleId mid, astIdx: aidx, token: TokenIndex id)
  c.CompilationData.modules.add:
    ModuleData(
      id: ModuleId id,
      
    )

proc legacyAppendPNode*(c: Compilation; n: PNode): AstIndex =
  ## take `n` the `PNode`, from parsing, and append it to `c` the `ModuleAst`.
  ## tracks the `AstIndex` of various nodes being appended, this allows to have
  ## a depth first construction of the AST -- when traversed linearly.

  let
    kind = legacyNodeToAstKind(n)
    token = c.tokens.legacyAdd(n)
    nodeIdx = c.ast.reserveAstNode(kind, token)
    leftAstIdx = AstLeft c.ast.getNextAstIndex()
    (left, right, extraProcessing) = case kind
      of ankStmtList:
        # Q: Why don't we store the first stmt's index?
        # A: because it's always the subsequent ast node, but the next stmt, if
        #    present, and any following are not equidistant. so the next idx is
        #    assumed to be left and doesn't need to be encoded as it's implied
        #    by the `kind`.
        let
          extraDataStart = c.ast.extra.len # where we start inserting indices
          childCount = n.safeLen           # then one or more stmts
          stmtCount = childCount - 1       # total ast indices in extra data

        (AstLeft extraDataStart, AstRight stmtCount, npkIndexAllButFirstChild)
      of ankCallCmd:
        # Q: Why don't we store the callee's index?
        # A: command node is immediately followed by the callee ast node, we
        #    assume the next idx to be left and don't need to encode it, as
        #    it's implied by the `kind`
        let
          extraDataStart = c.ast.extra.len # where we start insert indices
          childCount = n.safeLen           # callee and then one or more args
          argsCount = childCount - 1       # total ast indices in extra data

        (AstLeft extraDataStart, AstRight argsCount, npkIndexAllButFirstChild)
      else:
        (emptyAstLeft, emptyAstRight, npkNoExtraData)

  c.ast[nodeIdx].left = left
  c.ast[nodeIdx].right = right

  for i, child in n.pairs:
    let
      firstChild = i == 0
      idx = c.legacyAppendPNode(child) # append the node

    # if we have to do extra processing like remember extra data, do that here
    case extraProcessing
    of npkNoExtraData:
      # we already appended it, ignore everything else
      discard
    of npkIndexAllButFirstChild:
      # this is for cases like `ankCallCmd` where the first child is
      # immediately following and doesn't need to be indexed, but the args are
      # varying distances apart and indexed as extra data.
      if not firstChild:
        c.ast.extra.add idx

  result = nodeIdx
