#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module only contains ModuleId, as it's required by all other `data_*`
## modules within newast.

type
  ModuleId* = int32
    ## id for this module within a project compilation
    ## XXX: refactor along with `ModuleAst` as this is mostly to shim with the
    ##      existing compiler instead of properly handling module vs module in
    ##      a package vs during a specific compilation.
  
  ModuleData* = object
    ## stores the AST and various intermediate representation data for a module
    ##
    ## distinct types should be used to provide limited access views to this
    ## type in order to protect it from unwanted mutation or data change
    ## lifecycle violations
    ## 
    ## fields are kept in the order of how they're populated/mutated over time
    id*: ModuleId
      ## id created whenever a module is encountered
    tokens*: TokenList
      ## list of tokens, mutated while tokenizing, then read thereafter
    ast*: Ast
      ## ast, mutated based on parsing tokens, then read thereafter

proc initModule*(id: ModuleId, hintLen: Natural): ModuleData =
  ## initialize a ModuleData, pre-allocate storage based on the `hintLen` to
  ## excessive allocations/moves.
  ## xxx: clean-up how size hinting works
  ModuleData(
    id:     id,
    ast:    initAst(hintLen),
    tokens: initTokenList(hintLen)
  )