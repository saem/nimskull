#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the core memory/data types for the compiler internals,
## it's part of an attempt to migrate the compiler towards a set of lowerings.
##
## The physical data model makes compromises for the sake of performance, this
## means that it does not always map cleanly to the logical model it supports.
## The logical model, or rather the information model, is conveyed by a set of
## `distinct` types that create logical views on top of the physical model.

import data_module, data_token, data_ast

from ".." / options import ConfigRef

type
  CompilationData* = object
    ## stores all the data for a particular compilation
    ##
    ## distinct types should be used to provide limited access views to this
    ## type in order to protect it from unwanted mutation or data change
    ## lifecycle violations
    ## 
    ## fields are kept in the order of how they're populated/mutated over time
    ##
    # Future Considerations:
    # Extend or store this within a multiple-compilation context, that can be
    # used to do compilation or suggestion servers
    legacyConfig: ConfigRef
    ast: Ast
      ## AST for the entire project, to be run through the interpreter
    tokens: TokenList
      ## list of tokens, mutated while tokenizing, then read thereafter
    modules: seq[ModuleData]
      ## an index of modules encountered in the current compilation
