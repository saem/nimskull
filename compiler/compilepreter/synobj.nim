## syntax object module, primarly data types, and some convenience routines

from compiler/ast/idents import PIdent

type
  # xxx: resort these based on data storage, or the type of syntax node? Maybe
  #      keep consistent with 
  SyntaxKind* = enum
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
  synUptoUnaryKinds = {synDiscardStmt, synReturnStmt, synContinueStmt,
                       synBreakStmt, synRaiseStmt, synRefTy, synVarTy,
                       synPtrTy}
    # xxx: split these into dedicated kinds leaf vs unary
  synUnaryKinds = {synCallStrLit, synYieldStmt, synBind, synStaticStmt,
                   synMutableTy}
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
                   synFromStmt, synIncludeStmt, synExportStmt,
                   synExportExceptStmt, synConstSection, synConstDef,
                   synLetSection, synVarSection, synTypeSection, synTypeDef,
                   synEnumTy, synObjectTy, synTupleTy, synProcTy,
                   synIteratorTy, synRecList, synRecCase, synRecWhen,
                   synTypeOfExpr, synStaticTy, synDistinctTy, synTypeClassTy,
                   synOfInherit, synArgList, synWith, synWithout, synAsmStmt,
                   synUsingStmt}
    ## syntax nodes with 'n' kids (variable width)
    ## xxx: this category can likely be broken down further after fixing the
    ##      rest of the compiler and refining the structures

type
  # IdentIdx = distinct int32 # xxx: reusing ident cache/PIdent for now
  IdentIdx = PIdent
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
      of synCustomLit:
                            litPart:     StrIdx
                            suffix:      IdentIdx
      of synUptoUnaryKinds: unary: bool ## if true next node is child
      of synLeafKinds:      discard
      of synUnaryKinds:
        ## next node is the child
        discard
      of synBinaryKinds:
                            rightId: TreeIdx # left always follows the parent
      of synRoutineDefKinds:routineExtraId: ExtraIdx
      of synNKidsKinds:
                            extraId: ExtraIdx
                            count: Natural

  SyntaxDatum* = object
    ## an abstract syntax tree as a singular unit of data (datum).
    tree: seq[SyntaxDatumNode]
    extra: seq[TreeIdx]
    # ident: seq[string]       # xxx: reusing ident cache/PIdent for now
    strs: seq[string]

  # xxx: might drop all this source stuff, will see if required

  ParsedSourceId = int32

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


when false:
  from compiler/ast/ast_types import PNode, TNodeKind, nodeKindsProducedByParse

  proc toSyntaxDatum*(s: PNode): SyntaxDatum =
    ## legacy integration - mostly so we can auto-import the system module in
    ## `sem.semStmtAndGenerateGenerics`
    # xxx: should this be an "feAutoImport" instead so we can differentiate it,
    #      do we save some data to track it, or do we ignore it all together?
    var
      nodeStack: seq[PNode]
      syn: SyntaxDatum
      n = s
    while n != nil:
      case n.kind
      of nkEmpty: syn.tree.add(SyntaxDatumNode(kind: synEmpty))
      of nkIdent:
        syn.tree.add(SyntaxDatumNode(kind: synIdent, ident: n.ident))
      of nkCharLit: syn.tree.add(SyntaxDatumNode(kind: synCharLit, charLit: n.intVal.char))
      of nkIntLit: syn.tree.add(SyntaxDatumNode(kind: synIntLit, intLit: n.intVal.BiggestInt))
      of nkInt8Lit: syn.tree.add(SyntaxDatumNode(kind: synInt8Lit, int8Lit: n.intVal.int8))
      of nkInt16Lit: syn.tree.add(SyntaxDatumNode(kind: synInt16Lit, int16Lit: n.intVal.int16))
      of nkInt32Lit: syn.tree.add(SyntaxDatumNode(kind: synInt32Lit, int32Lit: n.intVal.int32))
      of nkInt64Lit: syn.tree.add(SyntaxDatumNode(kind: synInt64Lit, int64Lit: n.intVal.int64))
      of nkUIntLit: syn.tree.add(SyntaxDatumNode(kind: synUIntLit, uintLit: n.intVal.BiggestUInt))
      of nkUInt8Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt8Lit, uint8Lit: n.intVal.uint8))
      of nkUInt16Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt16Lit, uint16Lit: n.intVal.uint16))
      of nkUInt32Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt32Lit, uint32Lit: n.intVal.uint32))
      of nkUInt64Lit: syn.tree.add(SyntaxDatumNode(kind: synUInt64Lit, uint64Lit: n.intVal.uint64))
      of nkFloatLit: syn.tree.add(SyntaxDatumNode(kind: synFloatLit, floatLit: n.floatVal.BiggestFloat))
      of nkFloat32Lit: syn.tree.add(SyntaxDatumNode(kind: synFloat32Lit, float32Lit: n.floatVal.float32))
      of nkFloat64Lit: syn.tree.add(SyntaxDatumNode(kind: synFloat64Lit, float64Lit: n.floatVal.float64))
      of nkFloat128Lit: syn.tree.add(SyntaxDatumNode(kind: synFloat128Lit, float128Lit: n.floatVal.BiggestFloat))
      of nkStrLit: syn.tree.add(SyntaxDatumNode(kind: synStrLit,
                                                strLit: (let idx = syn.strs.len; syn.strs.add n.strVal; StrIdx idx)))
      of nkRStrLit: syn.tree.add(SyntaxDatumNode(kind: synRStrLit,
                                                strLit: (let idx = syn.strs.len; syn.strs.add n.strVal; StrIdx idx)))
      of nkTripleStrLit: syn.tree.add(SyntaxDatumNode(kind: synTripleStrLit,
                                                strLit: (let idx = syn.strs.len; syn.strs.add n.strVal; StrIdx idx)))
      of nkNilLit: syn.tree.add(SyntaxDatumNode(kind: synNilLit))
      of nkCustomLit: synCustomLit
      of nkAccQuoted: synAccQuoted
      of nkCall: synCall
      of nkCommand: synCommand
      of nkCallStrLit: synCallStrLit
      of nkInfix: synInfix
      of nkPrefix: synPrefix
      of nkPostfix: synPostfix
      of nkExprEqExpr: synExprEqExpr
      of nkExprColonExpr: synExprColonExpr
      of nkIdentDefs: synIdentDefs
      of nkConstDef: synConstDef
      of nkVarTuple: synVarTuple
      of nkPar: synPar
      of nkSqrBracket: synSqrBracket
      of nkCurly: synCurly
      of nkTupleConstr: synTupleConstr
      of nkObjConstr: synObjConstr
      of nkTableConstr: synTableConstr
      of nkSqrBracketExpr: synSqrBracketExpr
      of nkCurlyExpr: synCurlyExpr
      of nkPragmaExpr: synPragmaExpr
      of nkPragma: synPragma
      of nkPragmaBlock: synPragmaBlock
      of nkDotExpr: synDotExpr
      of nkIfExpr: synIfExpr
      of nkIfStmt: synIfStmt
      of nkElifBranch: synElifBranch
      of nkElifExpr: synElifExpr
      of nkElse: synElse
      of nkElseExpr: synElseExpr
      of nkCaseStmt: synCaseStmt
      of nkOfBranch: synOfBranch
      of nkWhenExpr: synWhenExpr
      of nkWhenStmt: synWhenStmt
      of nkForStmt: synForStmt
      of nkWhileStmt: synWhileStmt
      of nkBlockExpr: synBlockExpr
      of nkBlockStmt: synBlockStmt
      of nkDiscardStmt: synDiscardStmt
      of nkContinueStmt: synContinueStmt
      of nkBreakStmt: synBreakStmt
      of nkReturnStmt: synReturnStmt
      of nkRaiseStmt: synRaiseStmt
      of nkYieldStmt: synYieldStmt
      of nkTryStmt: synTryStmt
      of nkExceptBranch: synExceptBranch
      of nkFinally: synFinally
      of nkDefer: synDefer
      of nkLambda: synLambda
      of nkDo: synDo
      of nkBind: synBind
      of nkBindStmt: synBindStmt
      of nkMixinStmt: synMixinStmt
      of nkCast: synCast
      of nkStaticStmt: synStaticStmt
      of nkAsgn: synAsgn
      of nkGenericParams: synGenericParams
      of nkFormalParams: synFormalParams
      of nkStmtList: synStmtList
      of nkStmtListExpr: synStmtListExpr
      of nkImportStmt: synImportStmt
      of nkImportExceptStmt: synImportExceptStmt
      of nkFromStmt: synFromStmt
      of nkIncludeStmt: synIncludeStmt
      of nkExportStmt: synExportStmt
      of nkExportExceptStmt: synExportExceptStmt
      of nkConstSection: synConstSection
      of nkLetSection: synLetSection
      of nkVarSection: synVarSection
      of nkProcDef: synProcDef
      of nkFuncDef: synFuncDef
      of nkMethodDef: synMethodDef
      of nkConverterDef: synConverterDef
      of nkIteratorDef: synIteratorDef
      of nkMacroDef: synMacroDef
      of nkTemplateDef: synTemplateDef
      of nkTypeSection: synTypeSection
      of nkTypeDef: synTypeDef
      of nkEnumTy: synEnumTy
      of nkEnumFieldDef: synEnumFieldDef
      of nkObjectTy: synObjectTy
      of nkTupleTy: synTupleTy
      of nkProcTy: synProcTy
      of nkIteratorTy: synIteratorTy
      of nkRecList: synRecList
      of nkRecCase: synRecCase
      of nkRecWhen: synRecWhen
      of nkTypeOfExpr: synTypeOfExpr
      of nkRefTy: synRefTy
      of nkVarTy: synVarTy
      of nkPtrTy: synPtrTy
      of nkStaticTy: synStaticTy
      of nkDistinctTy: synDistinctTy
      of nkMutableTy: synMutableTy
      of nkTupleClassTy: synTupleClassTy
      of nkTypeClassTy: synTypeClassTy
      of nkOfInherit: synOfInherit
      of nkArgList: synArgList
      of nkWith: synWith
      of nkWithout: synWithout
      of nkAsmStmt: synAsmStmt
      of nkCommentStmt: synCommentStmt
      of nkUsingStmt: synUsingStmt
      of {low(TNodeKind) .. high(TNodeKind)} - nodeKindsProducedByParse:
        discard
