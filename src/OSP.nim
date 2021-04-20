## Nim0 lnaguage in Nim
## ====================
## The parser.
##
## From the syntactic items identified by the scanner (OSS.nim) reading the Nim0
## source file, the parser checks that Nim0 syntax is repected and deduces the
## role of the items. It can then call the RISC code generator (OSG.nim) to emit
## the corresponding RISC instructions.

import OSS, OSG
import strutils, hashes

const
  ## Standard procedures and functions
  ospStdOrd = 0           ## Ord of numeral
  ospStdChr = 1           ## Convert to character
  ospStdEot = 2           ## End of text is reached (no more parameters in command line)
  ospStdSwitch = 3        ## Read state of switches
  ospStdOpenInput = 4     ## OpenInput (not required in Nim0)
  ospStdReadInt = 5       ## ReadInt from command line
  ospStdRead = 6          ## Generic read from standard input
  ospStdWriteInt = 7      ## Write integer to standard output
  ospStdWriteChar = 8     ## Write `chr(x)` to standard output
  ospStdWrite = 9         ## Generic write to standard output
  ospStdWriteLine = 10    ## End line
  ospStdLED = 11          ## Show `x` on LED (not implemented)
  ospStdWriteBool = 12    ## Write boolean to standard output


var
  ospSym: int32           ## The current symbol being parsed [CC14]. It denotes a global
                          ## variable always representing the symbol last read from the
                          ## source text by a call to procedure `ospNext`.
  ospLevel: int32         ## The current level when using imbricated procedures [CC73].
  ospTopScope: osgObject  ## Head of the list of scopes [CC44].
  ospDummy: osgObject     ## When a symbol can't be found in the list of symbols
                          ## a ospDummy object is returned (instead of `nil`).
  W: File = stderr        ## Write compiler errors on standard error output.

# Forward references
proc ospExpression(x: var osgItem)
proc ospStatSequence


proc ospNewObj(obj: var osgObject; class: int32) =
  ## Creates a new object in the *symbol table* (in fact a list of objects).
  ## The new object is identified by `ossId` and is of type `class`. The
  ## resulting object is given by `obj` [CC40].
  ## 
  ## The procedure checks wheter the new identifier `ossId` is already present
  ## in the list. This would signify a multiple definition and constitude a
  ## programming error. The new entry is appended at the end of the list so
  ## that the list mirrors the order of the declarations in the source text.
  var x = ospTopScope
  while (x.next != nil) and (x.next.name != ossId):
    x = x.next
  if x.next == nil:
    let newObject = osgObject(name: ossId, class: class, next: nil)
    x.next = newObject
    obj = newObject
  else:
    obj = x.next
    ossMark("Multiple definition of " & ossId)


proc ospFind(obj: var osgObject) =
  ## Find the object with name `ossId` in the symbol table (the list of
  ## objects). If found, returns it in the `obj` variable. If not found,
  ## a ospDummy constant object is returned instead of `nil` [CC40].
  ## 
  ## It represents a simple linear search, proceeding through the list of
  ## scopes, and in each scope through the list of objects.
  var
    x: osgObject
    s = ospTopScope

  until (x != nil) or (s == nil):
    x = s.next
    while (x != nil) and (x.name != ossId):
      x = x.next
    s = s.dsc
  if x == nil:
    x = ospDummy
    ossMark(ossId & " is undefined")
  obj = x


proc ospFindField(obj: var osgObject; list: osgObject) =
  ## Find the field with name `ossId` in an `object`. `list` is the list
  ## of all fields of the object. If found, returns it in the `obj`
  ## parameter. If not found, returns the ospDummy object instead of `nil`.
  var l = list
  while (l != nil) and (l.name != ossId):
    l = l.next
  if l != nil:
    obj = l
  else:
    ossMark(ossId & " is undefined")
    obj = ospDummy


proc ospCheck(s: int; msg: string) =
  ## Check that the next symbol is the keyword `s`. If that's not the case,
  ## then prints the error message `msg`.
  if ospSym == s:
    ossGet(ospSym)
  else:
    ossMark(msg)


proc ospCheckInt(x: var osgItem) =
  ## Check that `x` is of type integer, else prints an error message.
  if x.typ.form != osgInteger:
    ossMark("Integer expected: " & $x.typ.form)


proc ospCheckBool(x: var osgItem) =
  ## Check that `x` is of type boolean, else prints an error message.
  if x.typ.form != osgBoolean:
    ossMark("Boolean expected: " & $x.typ.form)


proc ospCheckChar(x: var osgItem) =
  ## Check that `x` is of type character, else prints an error message.
  if x.typ.form != osgCharacter:
    ossMark("Character expected: " & $x.typ.form)


proc ospOpenScope =
  ## Creates a new scope [CC44].
  ## Ensures that the list of of records fields is not intermixed with the
  ## list of variables. Every record declaration established a new scope
  ## of visibiity of field identifiers. Note that the list into which new
  ## entries are inserted is rooted in the global variable `ospTopScope`.
  ospTopScope = osgObject(class: osgHead, dsc: ospTopScope, next: nil)


proc ospCloseScope =
  ## Close a scope [CC44].
  ospTopScope = ospTopScope.dsc


#[  -------------------- Parser --------------------- ]#

proc ospSelector(x: var osgItem) =
  ## Access to elements of structured variables, like `array` or `object` [CC57].
  ## selector = {"." ident | "[" expression "]"}.
  var
    y: osgItem
    obj: osgObject
  while (ospSym == ossLbrak) or (ospSym == ossPeriod):
    if ospSym == ossLbrak:
      ossGet(ospSym)
      ospExpression(y)
      if x.typ.form == osgArray:
        ospCheckInt(y)
        osgIndex(x, y)
        x.typ = x.typ.base
      else:
        ossMark(ossId & " is not an array")
      ospCheck(ossRbrak, "Missing ] for array element access in " & ossId)
    else: #[ period ]#
      ossGet(ospSym)
      if ospSym == ossIdent:
        if x.typ.form == osgRecord:
          ospFindField(obj, x.typ.dsc)
          ossGet(ospSym)
          if obj != nil:
            osgField(x, obj)
            x.typ = obj.typ
        else:
          ossMark(ossId & " is not an object")
      else:
        ossMark("Syntax error. Is " & ossId & " an identifier?")


proc ospCompTypes(t0, t1: osgType): bool =
  ## Check recursiveley if types `t0` and `t1` are compatible [CC92].
  result = (t0 == t1) or
           (t0.form == osgArray) and (t1.form == osgArray) and ospCompTypes(t0.base, t1.base)


proc ospParameter(par: osgObject) = 
  ## Parse a parameter in a procedure call [CC78].
  var x: osgItem
  ospExpression(x)
  if par != nil:
    let varpar = (par.class == osgPar)
    if ospCompTypes(par.typ, x.typ):
      if not varpar: # Call by value
        osgValueParam(x)
      else: # Call by reference
        osgVarParam(x, par.typ)

    # Call by openarray
    elif (x.typ.form == osgArray) and (par.typ.form == osgArray) and
         (x.typ.base.form == par.typ.base.form) and (par.typ.len < 0):
      osgOpenArrayParam(x)
    else:
      ossMark("Incompatible parameters " & par.name & " and " & ossId)


proc ospParamList(obj: var osgObject) =
  ## Parse the list of parameters in a procedure call [CC78].
  ## 
  ## ```
  ## actualParameters = "(" [expression {"," expression}] ")".
  ## ```
  var
    n: int = 0
    par: osgObject = obj.dsc
  
  if ospSym != ossRparen:
    ospParameter(par)
    n = 1
    while ospSym <= ossComma:
      ospCheck(ospSym, "Missing comma between parameters in procedure call")
      if par != nil:
        par = par.next
      inc(n)
      ospParameter(par)
    ospCheck(ossRparen, "Missing ) in procedure call")
  else:
    ossGet(ospSym)
  if n < obj.nofpar:
    ossMark("Too few parameters for procedure call")
  elif n > obj.nofpar:
    ossMark("Too many parameters for procedure call")


proc ospStandFunc(x: var osgItem; fctno: int32) =
  ## Parse standard functions call known by the compiler [CC80].
  ## `fctno` is the identifier of the procedure while `x` is the location
  ## where the result will be stored. Theses functions share the common
  ## property that they correspond either to a single instruction or to a short
  ## sequence of instructions. Therefore, they are handled differently by the
  ## compiler and no call using the stack is generated. Instead, the necessary
  ## instructions are emitted directly into the code (in-line functions).
  ## 
  ## Standard functions defined by Nim0 compiler:
  ## 
  ## - `ord`: Return the ordinal value of the character parameter.
  ## - `eot`: Return 1 if at end of text input.
  ## - `switch`: Not defined in Nim0
  if ospSym == ossLparen:
    ossGet(ospSym)
    if fctno == ospStdOrd: # ord: the ordinal number
      ospExpression(x)
      if x.typ != osgCharType:
        ossMark("ord argument is not of char type")
      osgOrd(x)
    elif fctno == ospStdChr: # chr: convert to character
      ospExpression(x)
      if x.typ != osgIntType:
        ossMark("chr argument is not of int type")
      osgChr(x)
    elif fctno == ospStdEot: # eot: end of text input
      osgEot(x)
    elif fctno == ospStdSwitch: # switch: read value from switch
      osgSwitch(x)
    else:
      ossMark("Unknown standard function " & ossId)

    if ospSym == ossRparen:
      ossGet(ospSym)
    else:
      ossMark("Right ) expected")
  else:
    ossMark("Parameter missing for standard function call")
    osgMakeConstItem(x, osgIntType, 0)


proc ospFactor(x: var osgItem) =
  ## Parse a factor in an expression [CC51].
  ## 
  ## ```
  ## factor = ident selector | number | "(" expression ")" | "not" factor.
  ## ```
  ## 
  ## In fact, the parser accepts more input than specified in the grammar:
  ## 
  ## - single characters are converted to integers.
  ## - `true` and `false` special constants are recognized too.
  var obj: osgObject

  # Sync parser in case of previous error
  if ospSym notin {ossInt, ossChar, ossIdent, ossLparen, ossRparen, ossNot, ossFalse, ossTrue}:
    ossMark("Expression expected; got " & ossId)
    until ospSym in {ossInt, ossChar, ossIdent, ossLparen, ossRparen, ossNot, ossFalse, ossTrue} or not ossIndentEqual():
      ossGet(ospSym)
      if ospSym == ossEof:
        ossMark("End of file reached")
        quit(QuitFailure)
  
  if ospSym == ossIdent:
    ospFind(obj)
    ossGet(ospSym)
    if obj.class == osgSFunc: # Standard functions known by compiler
      if obj.typ == nil:
        ossMark(ossId & "is not a function")
        obj.typ = osgIntType
      ospStandFunc(x, obj.val)
      x.typ = obj.typ
    else: # Symbol
      osgMakeItem(x, obj, ospLevel)
      ospSelector(x)
  elif ospSym == ossInt: # Integer constant
    osgMakeConstItem(x, osgIntType, ossVal)
    ossGet(ospSym)
  elif ospSym == ossChar: # Character constant
    osgMakeConstItem(x, osgCharType, ossVal)
    ossGet(ospSym)
  elif ospSym == ossLparen: # Expression
    ossGet(ospSym)
    if ospSym != ossRparen:
      ospExpression(x)
    ospCheck(ossRparen, "Missing ) to close expression")
  elif ospSym == ossNot: # Negation
    ossGet(ospSym)
    ospFactor(x)
    ospCheckBool(x)
    osgNot(x)
  elif ospSym == ossFalse: # False constant
    ossGet(ospSym)
    osgMakeConstItem(x, osgBoolType, 0)
  elif ospSym == ossTrue: # True constant
    ossGet(ospSym)
    osgMakeConstItem(x, osgBoolType, 1)
  else:
    ossMark("Factor expected; got " & ossId)
    osgMakeItem(x, ospDummy, ospLevel)


proc ospTerm(x: var osgItem) =
  ##  Parse *multiplicative* terms in an expression [CC52] and [CC67].
  ## 
  ## ```
  ## term = factor {("*" | "div" | "mod" | "and") factor}.
  ## ```
  ospFactor(x)
  while ospSym in {ossTimes, ossDiv, ossMod, ossAnd}:
    var y: osgItem
    let op = ospSym
    ossGet(ospSym)
    if op == ossTimes:
      ospCheckInt(x)
      ospFactor(y)
      ospCheckInt(y)
      osgMulOp(x, y)
    elif (op == ossDiv) or (op == ossMod):
      ospCheckInt(x)
      ospFactor(y)
      ospCheckInt(y)
      osgDivOp(op, x, y)
    else: # op = ossAnd [CC67]
      ospCheckBool(x)
      osgAnd1(x)
      ospFactor(y)
      ospCheckBool(y)
      osgAnd2(x, y)


proc ospSimpleExpression(x: var osgItem) =
  ## Parse *additive* terms in an expression [CC52] and [CC68].
  ## 
  ## ```
  ## simpleExpression = ["+" | "-"] term {("+" | "-" | "or") term}.
  ## ```
  ## 
  ## Note that processing of `or` condition needs 2 code generations
  ## calls compared with `+` or `-`.
  if ospSym == ossPlus:
    ossGet(ospSym)
    ospTerm(x)
    ospCheckInt(x)
  elif ospSym == ossMinus:
    ossGet(ospSym)
    ospTerm(x)
    ospCheckInt(x)
    osgNeg(x)
  else:
    ospTerm(x)

  while ospSym in {ossPlus, ossMinus, ossOr}:
    var y: osgItem
    let op = ospSym
    ossGet(ospSym)
    if op == ossOr:
      osgOr1(x)
      ospCheckBool(x)
      ospTerm(y)
      ospCheckBool(y)
      osgOr2(x, y)
    else:
      ospCheckInt(x)
      ospTerm(y)
      ospCheckInt(y)
      osgAddOp(op, x, y)


proc ospExpression(x: var osgItem) =
  ## Parse a comparison expression [CC63]. The result is a boolean type.
  ## 
  ## ```
  ## expression = simpleExpression [("=" | "!=" | "<" | "<=" | ">" | ">=") simpleExpression].
  ## ```
  ospSimpleExpression(x)
  if ospSym in {ossEql, ossNeq, ossLss, ossLeq, ossGtr, ossGeq}:
    let op = ospSym
    ossGet(ospSym)
    var y: osgItem
    ospSimpleExpression(y)
    if x.typ == y.typ:
      osgRelation(op, x, y)
    else:
      ossMark("Incompatible types for comparison")
    x.typ = osgBoolType


proc ospStandProc(pno: int32) =
  ## Parsing of standard procedures known by the compiler [CC80].
  ##  `pno` is the identifier of the procedure. Theses procedures share the common
  ## property that they correspond either to a single instruction or to a short
  ## sequence of instructions. Therefore, they are handled differently by the
  ## compiler and no call using the stack is generated. Instead, the necessary
  ## instructions are emitted directly into the code (in-line procedures).
  ## 
  ## The standard procedures defined by the compiler:
  ## 
  ## - `openInput`: Not defined in Nim0
  ## - `readInt`: Read next integer from the command parameters.
  ## - `writeInt`: Write the integer parameter on standard output.
  ## - `writeChar`: Write the integer parameter on standard output as a character.
  ## - `LED`: Not defined in Nim0
  ## 
  ## Extra procs that were added:
  ## 
  ## - `write`: generic write depending on parameter type
  ## - `read`: generic read depending on parameter type
  var x: osgItem
  if pno == ospStdOpenInput:
    osgOpenInput()  # This procedure is a no-op in Nim0
  elif pno in {ospStdReadInt, ospStdWriteInt, ospStdWriteChar, ospStdLED, ospStdWriteBool, ospStdWrite}:
    if ospSym == ossLparen:
      ossGet(ospSym)
      ospExpression(x)
      if pno == ospStdReadInt: # Read input: readInt
        if x.typ != osgIntType:
          ossMark("readInt argument is not of int type")
        osgReadInt(x)
      elif pno == ospStdWriteInt: # Output integer: writeInt
        if x.typ != osgIntType:
          ossMark("writeInt argument is not of int type")
        osgWriteInt(x)
      elif pno == ospStdWriteChar: # Output character
        if x.typ != osgCharType:
          ossMark("writeChar argument is not of char type")
        osgWriteChar(x)
      elif pno == ospStdWriteBool: # Output boolean
        if x.typ != osgBoolType:
          ossMark("writeBool argument is not of bool type")
        osgWriteBool(x)
      elif pno == ospStdLED: # Activate LED
        osgLED(x)
      elif pno == ospStdWrite: # Generic write based on argument type
        if x.typ == osgCharType:
          osgWriteChar(x)
        elif x.typ == osgIntType:
          osgWriteInt(x)
        elif x.typ == osgBoolType:
          osgWriteBool(x)
        else:
          ossMark("write does not support this type")
      elif pno == ospStdRead: # Generic read based on argument type
        if x.typ == osgIntType:
          osgReadInt(x)
        else:
          ossMark("read does not support this type")
      if ospSym == ossRparen:
        ossGet(ospSym)
      else:
        ossMark("Missing closing )")
    else:
      ossMark("Missing opening (")
  elif pno == ospStdWriteLine: # Write line feed: writeLn
    # We support optional () for call
    if ospSym == ossLparen:
      ossGet(ospSym)
      if ospSym == ossRparen:
        ossGet(ospSym)
      else:
        ossMark("Missing closing )")
    osgWriteLn()
  else:
    ossMark("Compiler error: undefined standard procedure")


proc ospIfStatement =
  ## Parse an `if ... elif: ... else: ...` statement.
  ## 
  ## ```
  ## ifStatement = "if" expression ":" IND> statementSequence {IND= "elif" expression ":" IND> statementSequence} [IND= "else:" IND> statementSequence] DED.
  ## ```
  var
    x: osgItem
    L: int32

  ossGet(ospSym)
  ospExpression(x)
  ospCheckBool(x)
  osgCFJump(x)
  ospCheck(ossColon, "Missing : in if statement")
  ossIndentSup("statements must be indented in if")
  ospStatSequence()
  ossIndentPop()

  L = 0
  while ospSym == ossElif and ossIndentEqual():
    ossIndentEqual("elif are not aligned")
    ossGet(ospSym)
    osgFJump(L)
    osgFixLink(x.a)
    ospExpression(x)
    ospCheckBool(x)
    osgCFJump(x)
    if ospSym == ossColon:
      ossGet(ospSym)
    else:
      ossMark("Missing : in elif statement")
    ossIndentSup("statements must be indented in if")
    ospStatSequence()
    ossIndentPop()

  if ospSym == ossElse and ossIndentEqual():
    ossIndentEqual("else is not aligned")
    ossGet(ospSym)
    if ospSym == ossColon:
      ossGet(ospSym)
    else:
      ossMark("Missing : in else statement")
    osgFJump(L)
    osgFixLink(x.a)
    ossIndentSup("statements must be indented in else")
    ospStatSequence()
    ossIndentPop()
  else:
    osgFixLink(x.a)
  osgFixLink(L)


proc ospWhileStatement =
  ## Parse a while loop statement
  ## 
  ## ```
  ## whileStatement = "while" expression ":" IND> statementSequence DED.
  ## ```
  var x: osgItem

  ossGet(ospSym)
  let L = osgPc
  ospExpression(x)
  ospCheckBool(x)
  osgCFJump(x)
  ospCheck(ossColon, "Missing : in while loop")
  ossIndentSup("Statements must be indented in while loop")
  ospStatSequence()
  ossIndentPop()
  osgBJump(L)
  osgFixLink(x.a)


proc ospUntilStatement =
  ## Parse an until loop statement.
  ## Nim does not have such a control structure that is inherited from Oberon-0.
  ## It has the same syntax as the `until` template defined in this project.
  ## 
  ## ```
  ## untilStatement = "until" expression ":" IND> statementSequence DED.
  ## ```
  ## 
  ## This control structure is equivalent to:
  ## ```
  ## statement
  ## while not expression:
  ##   statement
  ## ```
  var x: osgItem

  ossGet(ospSym)
  let L = osgPc
  ospExpression(x)
  ospCheckBool(x)
  ospCheck(ossColon, "Missing : in until loop")
  ossIndentSup("Statements must be indented in until loop")
  ospStatSequence()
  ossIndentPop()
  osgCBJump(x, L)


proc ospForStatement =
  ## Parse a simple for loop on a range: `for i in start .. end:`. The loop
  ## variable `i` must be previously declared.
  ## 
  ## ```
  ## forStatement = "for" ident "in" expression ".." expression ":" IND> statementSequence DED.
  ## ```
  ## 
  ## The for loop defines a new scope where the loop variable is defined, The
  ## type of the loop variable is defined by the type of the range.
  ## ** NOT IMPLEMENTED **
  ## Because of lack of time, this was not implemented: look at `ospForStatement2`
  ## for current attempt. Presently, the loop iterator variable must be explicitely
  ## declared as a variable
  var
    obj: osgObject
    x, y, z, incr: osgItem
    L, M: int32
    rang: int32 = ossNull
  # Find variable and initialize with start
  ossGet(ospSym)
  ospFind(obj)
  ossGet(ospSym)
  ospCheck(ossIn, "Missing keyword in in for loop")
  ospSimpleExpression(y)
  if y.typ != nil:
    osgMakeItem(x, obj, ospLevel)
    if (x.typ.form in {osgBoolean, osgInteger, osgCharacter}) and (x.typ.form == y.typ.form):
      osgStore(x, y)
    else:
      ossMark("For loop only supports int, char or bool type ranges")
  else:
    ossMark("Loop expression has no type and can't be used in a for loop")
  # Read end loop value
  if ospSym notin {ossRange, ossRangeL}:
    ossMark("Only for loop on ranges are supported: for i in start .. end: / for i in start ..< end")
  else:
    rang = ospSym
  ossGet(ospSym)
  L = osgPc
  ospSimpleExpression(z)
  if z.typ != y.typ:
    ossMark("For loop start and end are not of the same type")
  # Check end condition: loop variable <= end
  if rang == ossRange:
    osgRelation(ossLeq, x, z)
  elif rang == ossRangeL:
    osgRelation(ossLss, x, z)
  else:
    ossMark("Only ranges .. and ..< are supported")
  x.typ = osgBoolType # Now we change x to boolean...
  osgCFJump(x)
  M = x.a
  # Statements
  ospCheck(ossColon, "Missing : in for loop")
  ossIndentSup("Statements must be indented in for loop")
  ospStatSequence()
  ossIndentPop()
  # Increment variable: i = i + 1
  osgMakeConstItem(incr, y.typ, 1)
  osgMakeItem(x, obj, ospLevel)
  osgAddOp(ossPlus, x, incr)
  osgMakeItem(z, obj, ospLevel)
  osgStore(z, x)
  osgBJump(L)
  osgFixLink(M)


proc ospForStatement2 =
  ## Parse a simple for loop on a range: `for i in start .. end:` with automatic
  ## type declaration for loop variable `i`.
  ## 
  ## **CAUTION: This code does not work!**
  ## 
  ## ```
  ## forStatement = "for" ident "in" expression ".." expression ":" IND> statementSequence DED.
  ## ```
  ## 
  ## The for loop defines a new scope where the loop variable is defined, The
  ## type of the loop variable is defined by the type of the range.
  var
    obj: osgObject
    x, y, z, incr: osgItem
    L, M: int32
    rang: int32 = ossNull
  # Create new scope
  ospOpenScope()
  inc(ospLevel)
  # Create variable and initialize with start
  ossGet(ospSym)
  ospNewObj(obj, osgVar)
  ossGet(ospSym)
  ospCheck(ossIn, "Missing keyword in in for loop")
  ospSimpleExpression(y)
  if y.typ != nil:
    if (y.typ.form in {osgInteger, osgCharacter}):
      # When we know the type of the range, we can create the loop variable with the same type
      obj.typ = y.typ
      obj.lev = ospLevel
      obj.val = obj.typ.size
      osgMakeItem(x, obj, ospLevel)
      osgStore(x, y)
    else:
      ossMark("The type of " & ossId & " can't be used in a for loop; for loop only support int or char type ranges")
  else:
    ossMark("Loop expression has no type and can't be used in a for loop")
  # Read end loop value
  if ospSym notin {ossRange, ossRangeL}:
    ossMark("Only for loop on ranges are supported: for i in start .. end: / for i in start ..< end")
  else:
    rang = ospSym
  ossGet(ospSym)
  L = osgPc
  ospSimpleExpression(z)
  if z.typ != y.typ:
    ossMark("For loop start and end are not of the same type")
  # Check end condition: loop variable <= end
  if rang == ossRange:
    osgRelation(ossLeq, x, z)
  elif rang == ossRangeL:
    osgRelation(ossLss, x, z)
  else:
    ossMark("Only ranges .. and ..< are supported")
  x.typ = osgBoolType # Now we change x to boolean...
  osgCFJump(x)
  M = x.a
  # Statements
  ospCheck(ossColon, "Missing : in for loop")
  ossIndentSup("Statements must be indented in for loop")
  ospStatSequence()
  ossIndentPop()
  # Increment variable: i = i + 1
  osgMakeConstItem(incr, y.typ, 1)
  osgMakeItem(x, obj, ospLevel)
  osgAddOp(ossPlus, x, incr)
  osgMakeItem(z, obj, ospLevel)
  osgStore(z, x)
  osgBJump(L)
  osgFixLink(M)
  # Close scope
  dec(ospLevel)
  ospCloseScope()


proc ospStatSequence =
  ## Recursive procedure to parse sequence of statements [CC62] and [CC78].
  ## 
  ## ```
  ## assignment = ident selector "-" expression.
  ## actualParameters = "(" [expression {"," expression}] ")".
  ## procedureCall = ident [actualParameters].
  ## statement = [assignment | procedureCall | ifStatement | whileStatement | untilStatement | forStatement].
  ## statementSequence = statement {IND= statement}.
  ## ```
  ## 
  ## The procedure is recursive as it emits branch instructions whose
  ## destination adresses must be fixed up when known. Using recursion allows
  ## stacking environments.
  ## We've broken Wirth's procedure by using forward references.
  ## 
  ## We also kept the `until` control structure from Oberion-0 that does not
  ## exists in Nim.
  var
    obj: osgObject
    x, y: osgItem

  # Synchronize the parser in case an error occurred [CC35].
  until not ossIndentEqual() or ospSym == ossEof:
    if ospSym == ossEof:
      return

    if ospSym notin {ossIdent, ossIf, ossWhile, ossFor, ossRepeat}:
      ossMark("Statement expected")
      until ospSym in {ossIdent, ossIf, ossWhile, ossFor, ossRepeat}:
        ossGet(ospSym)
        if ospSym == ossEof:
          ossMark("End of file reached")
          quit(QuitFailure)
    
    obj = nil
    # Identifier encountered: it could be the begining of an assignment or a
    # procedure call.
    if ospSym == ossIdent:
      ospFind(obj)
      ossGet(ospSym)
      if obj.class == osgSProc: # Standard system procedure
        ospStandProc(obj.val)
      else:
        osgMakeItem(x, obj, ospLevel)
        ospSelector(x)
        if ospSym == ossBecomes: # Assignment
          ossGet(ospSym)
          ospExpression(y)
          if y.typ != nil:
            if (x.typ.form in {osgBoolean, osgInteger, osgCharacter}) and (x.typ.form == y.typ.form):
              osgStore(x, y)
            else:
              ossMark("Incompatible assignment")
          else:
            ossMark("Expression has no type and can't be assigned")
        elif ospSym == ossLparen: # Procedure call [CC78]
          ossGet(ospSym)
          if (obj.class == osgProc) and (obj.typ == nil):
            ospParamList(obj)
            osgCall(obj)
          else:
            ossMark("Procedure " & obj.name & " is not defined")
        elif obj.class == osgProc: # Procedure call without parameters
          if obj.nofpar > 0:
            ossMark("Missing parameters for procedure " & obj.name)
          if obj.typ == nil:
            osgCall(obj)
          else:
            ossMark("Procedure " & obj.name & " is not defined")
        elif obj.class == osgTyp:
          ossMark("Illegal assignment; " & obj.name & " is a type")
        else:
          ossMark("Procedure " & obj.name & " is not defined")

    # If statement [CC63].
    elif ospSym == ossIf:
      ospIfStatement()

    # While statement [CC65].
    elif ospSym == ossWhile:
      ospWhileStatement()

    # Repeat until statement [CC65].
    elif ospSym == ossRepeat:
      ospUntilStatement()

    # For loop. Only the range style `for i in start .. end:`
    elif ospSym == ossFor:
      ospForStatement()

    # Control: check that we haven't saturated RISC registers
    osgCheckRegs()


proc ospIdentList(class: int32; first: var osgObject) =
  ## Process a list of identifiers of the given class and return the first
  ## object of the list in `first` [CC43].
  ## 
  ## ```
  ## identList = ident {"," ident}.
  ## ```
  var obj: osgObject
  if ospSym == ossIdent:
    ospNewObj(first, class)
    ossGet(ospSym)
    while ospSym == ossComma:
      ossGet(ospSym)
      if ospSym == ossIdent:
        ospNewObj(obj, class)
        ossGet(ospSym)
      else:
        ossMark("Identier expected; got " & ossId)
    ospCheck(ossColon, "Missing colon :")


proc ospType(objType: var osgType) =
  ## Recursively compiles a type declaration [CC43].
  ## 
  ## ```
  ## arrayType = "array" "[" expression "," type "]".
  ## fieldList = [identList ":" type].
  ## objectType = "object" IND> fieldList {"," fieldList} DED.
  ## type = ident | arrayType | objectType.
  ## ```
  var
    obj, firstObj: osgObject
    x: osgItem
    tp: osgType
  
  # Parser synchronization in case of previous error
  objType = osgIntType
  if ospSym notin {ossIdent, ossArray, ossObject}:
    ossMark("Type expected; got " & ossId)
    until ospSym in {ossIdent, ossArray, ossObject} or not ossIndentEqual():
      ossGet(ospSym)
      if ospSym == ossEof:
        ossMark("End of file reached")
        quit(QuitFailure)

  # Named type
  if ospSym == ossIdent:
    ospFind(obj)
    ossGet(ospSym)
    if obj.class == osgTyp:
      objType = obj.typ
    else:
      ossMark("Type expected; got " & ossId)
  # Array
  elif ospSym == ossArray:
    ossGet(ospSym)
    if ospSym != ossLbrak:
      ossMark("Missing [ in array declaration")
    else:
      ossGet(ospSym)
    ospExpression(x)
    if (x.mode != osgConst) or (x.a < 0):
      ossMark("Bad array index")
    if ospSym == ossComma:
      ossGet(ospSym)
    else:
      ossMark("Missing , in array declaration")
    ospType(tp)
    objType = osgType(form: osgArray, base: tp, len: x.a, size: x.a * tp.size)
    if ospSym != ossRbrak:
      ossMark("Missing ] in array declaration")
    else:
      ossGet(ospSym)
  # Object
  elif ospSym == ossObject:
    ossGet(ospSym)
    objType = osgType(form: osgRecord, size: 0)
    ospOpenScope()
    var first = true
    until ospSym != ossIdent and ossIndentEqual():
      if ospSym == ossIdent:
        if first:
          ossIndentSup("object fields must be indented")
          first = false
        else:
          ossIndentEqual("object fields must be aligned")
        ospIdentList(osgFld, firstObj)
        ospType(tp)
        obj = firstObj
        while obj != nil:
          obj.typ = tp
          obj.val = objType.size
          objType.size = objType.size + obj.typ.size
          obj = obj.next
    ossIndentPop()
    ospCloseScope()
  else:
    ossMark("Missing type identifier? Got " & ossId)  


proc ospDeclarations(varsize: var int32) =
  ## Parse local the declaration sections for `const`, `type` and `var`,
  ## or variables declarations in a procedure or global ones
  ## in a module [CC77]. Returns in `varsize` the size of the variables that
  ## must be allocated to store the variables in the stack (in case of 
  ## procedure).
  ## 
  ## ```
  ## declarations = ["const" IND> {IND= ident "=" expression} DED] |
  ##                ["type" IND> {IND= ident "=" type} DED] |
  ##                ["var" IND> {IND=identList ":" type} DED] |
  ##                {procedureDeclaration}.
  ## ```
  var
    obj, firstObj: osgObject
    x: osgItem
    tp: osgType
  # Parser synchronisation in case of previous error
  if ospSym notin {ossConst, ossType, ossVar}:
    ossMark("Local declarations (const, type, var) expected")
    until ospSym in {ossConst, ossType, ossVar} or not ossIndentEqual():
      ossGet(ospSym)
      if ospSym == ossEof:
        ossMark("End of file reached")
        quit(QuitFailure)


  while ospSym in {ossConst, ossType, ossVar}:
    # Local constants
    if ospSym == ossConst:
      ossGet(ospSym)
      ossIndentSup("const values must be indented")
      ospNewObj(obj, osgConst)
      ossGet(ospSym)
      if ospSym == ossBecomes:
        ossGet(ospSym)
      else:
        ossMark("Missing = to declare constant value")
      ospExpression(x)
      if x.mode == osgConst:
        obj.val = x.a
        obj.typ = x.typ
      else:
        ossMark("Expression not constant can't be used to declare constant")
      while ossIndentEqual():
        ospNewObj(obj, osgConst)
        ossGet(ospSym)
        if ospSym == ossBecomes:
          ossGet(ospSym)
        else:
          ossMark("Missing = to declare constant value")
        ospExpression(x)
        if x.mode == osgConst:
          obj.val = x.a
          obj.typ = x.typ
        else:
          ossMark("Expression not constant can't be used to declare constant")
      ossIndentPop()

    # Local types
    if ospSym == ossType:
      # First type definition
      ossGet(ospSym)
      ossIndentSup("type definitions must be indented")
      ospNewObj(obj, osgTyp)
      ossGet(ospSym)
      if ospSym == ossBecomes:
        ossGet(ospSym)
      else:
        ossMark("Missing = to define type")
      ospType(obj.typ)
      # Following type definitions
      while ossIndentEqual():
        ospNewObj(obj, osgTyp)
        ossGet(ospSym)
        if ospSym == ossBecomes:
          ossGet(ospSym)
        else:
          ossMark("Missing = to define type")
        ospType(obj.typ)
      ossIndentPop()

    # Local variables
    if ospSym == ossVar:
      # First variable declaration
      ossGet(ospSym)
      ossIndentSup("var declarations must be indented")
      ospIdentList(osgVar, firstObj)
      ospType(tp)
      obj = firstObj
      while obj != nil:
        obj.typ = tp
        obj.lev = ospLevel
        obj.val = varsize
        varsize = varsize + obj.typ.size
        obj = obj.next
      # Following variable declarations
      while ossIndentEqual():
        ospIdentList(osgVar, firstObj)
        ospType(tp)
        obj = firstObj
        while obj != nil:
          obj.typ = tp
          obj.lev = ospLevel
          obj.val = varsize
          varsize = varsize + obj.typ.size
          obj = obj.next
      ossIndentPop()


proc ospSetClass(class: int32; first: osgObject) =
  ## Set the class of the list of object to `class`.
  ## This is used because Nim0 call by-address formal parameters syntax is
  ## `x, y: var typ` while Oberon-0 syntax is `var x, y: typ`.
  var obj = first
  while obj != nil:
    obj.class = class
    obj = obj.next


proc ospProcedureDecl =
  ## Parses a procedure declaration [CC76].
  ## 
  ## ```
  ## procedureHeading = "proc" ident [formalParameters].
  ## procedureBody = [declaration | statementSequence].
  ## procedureDeclaration = procedureHeading "=" IND> procedureBody DED.
  ## ```
  var
    objProc: osgObject
    procid: string    # Name of the procedure or function
    nofpar: int32     # Number of formal parameters
    locblksize: int32 # Size of local variables block
    parblksize: int32 # Size of formal parameters block
    
  proc ospFPSection(adr: var int32; nofpar: var int32) =
    ## Parse the formal parameters of the procedure and creates the local linked
    ## list for the local context [CC76]. Structured parameters like `array` or
    ## `object` are not supported.
    ## 
    ## ```
    ## fpSection = identList ":" ["var"] type.
    ## formalParameters = "(" [fpSection {";" fpSection}] ")".
    ## ```
    var
      obj, first: osgObject
      tp: osgType
      parsize: int32

    ospIdentList(osgVar, first)
    if ospSym == ossVar:  # Change class of parameters from by-value to by-address
      ospSetClass(osgPar, first)
      ossGet(ospSym)
    if ospSym == ossIdent:
      ospFind(obj)
      ossGet(ospSym)
      if obj.class == osgTyp:
        tp = obj.typ
      else:
        ossMark("Type expected; got " & ossId)
        tp = osgIntType
    else:
      if ossId == "var":
        ossMark("Syntax for call by-address formal parameters is `x, y: var type` and not `var x, y: type`")
      else:
        ossMark("Identifier expected; got " & ossId)
      tp = osgIntType
    if first == nil:
      first = ospDummy
      
    if first.class == osgVar:
      parsize = tp.size
      if tp.form >= osgArray:
        ossMark("No structured parameters supported")
    else:
      parsize = osgWordSize
    obj = first
    while obj != nil:
      inc(nofpar)
      obj.typ = tp
      obj.lev = ospLevel
      obj.val = adr
      adr = adr + parsize
      obj = obj.next


  # ospProcedureDecl body
  ossGet(ospSym)
  if ospSym == ossIdent:
    procid = ossId
    ospNewObj(objProc, osgProc)
    ossGet(ospSym)
    parblksize = osgWordSize
    nofpar = 0
    ospOpenScope()
    inc(ospLevel)
    objProc.val = -1

    # Exported procedure (==> Command / Entry point)
    if ospSym == ossTimes:
      objProc.comd = true
      ossGet(ospSym)
    else:
      objProc.comd = false

    # Formal parameters
    if ospSym == ossLparen:
      ossGet(ospSym)
      if ospSym == ossRparen:
        ossGet(ospSym)
      else:
        ospFPSection(parblksize, nofpar)
        while ospSym == ossSemicolon:
          ossGet(ospSym)
          ospFPSection(parblksize, nofpar)
        if ospSym == ossRparen:
          ossGet(ospSym)
        else:
          ossMark("Missing ) when closing formal parameters list in proc")
        if objProc.comd:
          ossMark("No parameters allowed for external proc")

    locblksize = parblksize
    objProc.typ = nil
    objProc.dsc = ospTopScope.next
    objProc.nofpar = nofpar
    ospCheck(ossBecomes, "= expected")  # We don't support forward declarations

    objProc.dsc = ospTopScope.next

    ossIndentSup("proc body must be indented")

    # Declaration sections
    while ospSym in {ossConst, ossType, ossVar, ossProcedure}:
      if ospSym in {ossConst, ossType, ossVar}:
        # Local consts, types and variables declaration
        ospDeclarations(locblksize)
      else: # Local procedures declaration
        ospProcedureDecl()

    # Procedure prologue
    objProc.val = osgPc * osgWordSize
    osgEnter(parblksize, locblksize, objProc.comd)

    # Statements
    ospStatSequence()
    ossIndentPop()

    # Procedure epiloge
    osgReturn(locblksize)
    dec(ospLevel)
    ospCloseScope()


proc ospCalcKey(modid: string): int32 =
  ## Calculate module versioning key [CC97]. We do a simple hash on the module
  ## name without extension. There's no configuration consistency check here, as
  ## it should be done on source code or interface instead, but that enough for
  ## Nim0 where we don't deal with module imports. We could have used a constant
  ## result value but this was written when trying to respect Oberon object format...
  let m = if modid.endsWith(".nim0"): substr(modid, 0, len(modid) - 5) else: modid
  result = cast[int32](hash(m))


proc ospModule(modid: string) =
  ## Parse the main module (start symbol of parser) and produce the resulting
  ## code if no errors have been encountered. `modid` is the name of the file
  ## compiled.
  ## 
  ## ```
  ## module = declarations [statementSequence].
  ## ```
  var dc: int32 = 0 # The size of the memory stack that must be reserved for
                    # variables allocation.

  writeLine(W, "  compiling " & modid & ".nim0")

  osgOpen()
  ospOpenScope()
  ospLevel = 0

  while ospSym in {ossConst, ossType, ossVar, ossProcedure}:
    if ospSym in {ossConst, ossType, ossVar}: # Constants, types and global variables
      ospDeclarations(dc)
    elif ospSym == ossProcedure: # Procedure and functions
      ospProcedureDecl()
    else:
      ossMark("func, template and macro are not supported:" & ossId)

  osgHeader(dc) # Module prologue
  ospStatSequence()

  if not ossError:
    let key = ospCalcKey(modid)
    osgClose(modid, key, dc, ospTopScope)
    writeLine(W, "code generated ", modid, ", PC=", osgPc, ", dc=", dc, ", key=", key)
  else:
    writeLine(W, "CAUTION: Generated code is not complete and valid!")
  ospCloseScope()


proc ospEnter(name: string; cl: int32; n: int32; oType: osgType) =
  ## Add a new symbol in the table of symbols.
  ospTopScope.next = osgObject(class: cl, val: n, name: name, typ: oType, dsc: nil, next: ospTopScope.next)


proc ospInit =
  ## Initialize parser, creating the system symbols known by the compiler.
  ospDummy = osgObject(class: osgVar, typ: osgIntType, val: 0)
  ospTopScope = nil
  ospOpenScope()
  ospEnter("ord", osgSFunc, ospStdOrd, osgIntType)
  ospEnter("chr", osgSFunc, ospStdChr, osgCharType)
  ospEnter("eot", osgSFunc, ospStdEot, osgBoolType)
  ospEnter("Switch", osgSFunc, ospStdSwitch, osgIntType)
  ospEnter("openInput", osgSProc, ospStdOpenInput, nil)
  ospEnter("readInt", osgSProc, ospStdReadInt, nil)
  ospEnter("read", osgSProc, ospStdRead, nil)
  ospEnter("writeInt", osgSProc, ospStdWriteInt, nil)
  ospEnter("writeChar", osgSProc, ospStdWriteChar, nil)
  ospEnter("writeBool", osgSProc, ospStdWriteBool, nil)
  ospEnter("writeLine", osgSProc, ospStdWriteLine, nil)
  ospEnter("LED", osgSProc, ospStdLED, nil)
  ospEnter("write", osgSProc, ospStdWrite, nil)
  ospEnter("bool", osgTyp, 0, osgBoolType)
  ospEnter("int", osgTyp, 1, osgIntType)
  ospEnter("char", osgTyp, 2, osgCharType)


proc ospCompile*(target: string; verbose: bool): int =
  writeLine(W, "Nim0 Compiler v1.0")

  var m = target
  var t = target
  if target.endsWith(".nim0"):
    t = target[0 .. ^6]
  else:
    m = m & ".nim0"
  var T: File
  if open(T, m, fmRead):
    ospInit()
    ossInit(T, 0, W)
    ossGet(ospSym)
    ospModule(t)

    if verbose:
      osgDecode()
      if ossError:
        writeLine(W, "CAUTION: Generated code is not complete and valid!")
  else:
    writeLine(stderr, "Can't open source filename " & m)
  result = if ossError: QuitFailure else: QuitSuccess
