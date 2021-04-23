## Nim0 language in Nim
## ====================
## The scanner.
##
## Read each ossLine of the source file character by character, detecting keywords
## of the language and other syntactic items: numbers, comments, identifiers...

const
  ossIdLen = 16             ## Length of identifiers
  ossMaxInt = 2147483647    ## Size of maximum integer to detect overflows
  ossMaxSource = 999        ## Maximum number of rows in source file

  # Lexical symbols of Nim0 [CC32]
  ossNull* = 0'i32
  ossTimes* = 1'i32
  ossXor* = 2'i32
  ossDiv* = 3'i32
  ossMod* = 4'i32
  ossAnd* = 5'i32
  ossPlus* = 6'i32
  ossMinus* = 7'i32
  ossOr* = 8'i32
  ossEql* = 9'i32
  ossNeq* = 10'i32
  ossLss* = 11'i32
  ossLeq* = 12'i32
  ossGtr* = 13'i32
  ossGeq* = 14'i32
  ossRange* = 16'i32
  ossRangeL* = 17'i32
  ossPeriod* = 18'i32
  ossChar* = 20'i32
  ossInt* = 21'i32
  ossFalse* = 23'i32
  ossTrue* = 24'i32
  ossNot* = 27'i32
  ossLparen* = 28'i32
  ossLbrak* = 29'i32
  ossIdent* = 31'i32
  ossIf* = 32'i32
  ossIn* = 33'i32
  ossWhile* = 34'i32
  ossRepeat* = 35'i32
  ossFor* = 36'i32
  ossComma* = 40'i32
  ossColon* = 41'i32
  ossBecomes* = 42'i32
  ossLet* = 43'i32
  ossRparen* = 44'i32
  ossRbrak* = 45'i32
  ossThen* = 47'i32
  ossOf* = 48'i32
  ossDo* = 49'i32
  ossSemicolon* = 52'i32
  ossEnd* = 53'i32
  ossElse* = 55'i32
  ossElif* = 56'i32
  ossUntil* = 57'i32
  ossArray* = 60'i32
  ossObject* = 61'i32
  ossConst* = 63'i32
  ossType* = 64'i32
  ossVar* = 65'i32
  ossProcedure* = 66'i32
  ossBegin* = 67'i32
  ossModule* = 69'i32
  ossDollar* = 90'i32
  ossBang* = 91'i32
  ossPercent* = 92'i32

  ossEof* = 100'i32 # End of file


template until*(cond, body: untyped): untyped =
  ## Reproduces the `repeat` ... `until condition` loop that exists in Oberon.
  while true:
    body
    if cond:
      break

type
  Keyword = tuple         ## A reserved keyword in Nim0 syntax.
    sym: int32            ## An numeric identifier for the reserved keyword.
    id: string            ## The string value of the keyword.

  DebugInfo* = object     ## Source code information
    pc*: int              ## Address in generated code
    row*: int             ## Line number in source code
    line*: string         ## Line of source code

var
  ossIndents: array[50, int] ## The stack of indentation levels
  ossIi: int = 0          ## The index in the indentation stack `ossIndents`

  ossVal*: int32          ## The value when scanning an integer
  ossId*: string          ## The name of the current symbol
  ossIndent*: int         ## The current indentation level
  ossError*: bool = true  ## Have we encountered errors?

  ossCh: char             ## The character being read
  ossErrPos: int          ## The position of the current character in the source file.
  ossCol: int = 1         ## The column of the current character in the source file.
  ossRow: int = 1         ## The ossLine of the source file where is the current
                          ## character being read.

  R: File                 ## Input file: i.e. source code
  W: File                 ## Output: i.e. stdout

  ossLine: string = ""    ## The current ossLine being parsed
  ossIDebug: int = 0      ## Index in debug information array
  ossDebug*: array[ossMaxSource + 1, DebugInfo] ## Source debug information for verbose mode.

  ## Keywords of Nim0
  ## In case you add new features to Nim0, assign the symbol corresponding to
  ## the symbol that you need to define.
  keyTab: array[36, Keyword] = [
    (ossArray, "array"),
    (ossAnd, "and"),
    (ossNull, "BEGIN"),
    (ossNull, "block"),
    (ossConst, "const"),
    (ossDiv, "div"),
    (ossNull, "DO"),
    (ossElse, "else"),
    (ossElif, "elif"),
    (ossNull, "END"),
    (ossFalse, "false"),
    (ossFor, "for"),
    (ossIf, "if"),
    (ossNull, "import"),
    (ossIn, "in"),
    (ossNull, "is"),
    (ossNull, "let"),
    (ossMod, "mod"),
    (ossNull, "MODULE"),
    (ossNull, "nil"),
    (ossNot, "not"),
    (ossNull, "OF"),
    (ossOr, "or"),
    (ossNull, "pointer"),
    (ossProcedure, "proc"),
    (ossObject, "object"),
    (ossNull, "REPEAT"),
    (ossNull, "return"),
    (ossNull, "THEN"),
    (ossNull, "TO"),
    (ossTrue, "true"),
    (ossType, "type"),
    (ossUntil, "UNTIL"),
    (ossVar, "var"),
    (ossWhile, "while"),
    (ossNull, "xor"),
  ]

proc ossMark*(msg: string) =
  ## Print the error message `msg` with location information in source file.
  try:
    var p = int(getFilePos(R) - 1)
    if p > ossErrPos:
      writeLine(W, "[", ossRow, ", ", ossCol, "]@=", p, " ", msg, " (near id=", ossId, ", val=", ossVal, ")")
    ossErrPos = p
  except:
    writeLine(W, msg)
  ossError = true


proc ossSetDebug(row: int; line: string) =
  # When a source ossLine has been read, set the source debug information
  # for use later by decoder.
  ossDebug[ossIDebug].row = row
  ossDebug[ossIDebug].line = line
  inc(ossIDebug)
  ossDebug[ossIDebug].pc = -1 # Sentinelle


proc ossSetDebugPc*(pc: int32) =
  ## Save the program counter (PC) corresponding to a source ossLine,
  ## if not already saved. Else set the sentinel.
  if ossIDebug > 0 and ossDebug[ossIDebug - 1].pc <= 0:
    ossDebug[ossIDebug - 1].pc = pc
  else:
    # Set sentinel
    ossDebug[ossIDebug].pc = -1


proc ossIndentPush =
  ## Push the current indentation level into the stack of levels.
  inc(ossIi)
  ossIndents[ossIi] = ossIndent


proc ossIndentPop* =
  ## Pop the top indentation from the stack of levels
  dec(ossIi)


proc ossIndentSup*(msg: string = "") =
  ## Check that the current indentation level is higher than the top of the
  ## stack of levels, else prints the error message `msg`.
  if ossIndent > ossIndents[ossIi]:
    ossIndentPush()
  else:
    ossMark("Indentation error: " & (if msg != "": msg & "; " else: "" ) & ossId & " should be at column " & $(ossIndents[ossIi] + 2))


proc ossIndentEqual*(msg: string) =
  ## Check that the current indentation level is the same than the top of the
  ## stack of levels, else prints an error message.
  if ossIndent != ossIndents[ossIi]:
    ossMark("Indentation error: " & msg & "; " & ossId & " should be at column " & $ossIndents[ossIi])


proc ossIndentEqual*: bool =
  ## Check that the current identation level is the same than the top of the
  ## stack of levels, returning the result instead of printing an error message.
  result = ossIndent == ossIndents[ossIi]


proc ossReadC(f: File): char =
  ## Read a character from the source file `f`.
  ## 
  ## As a hack, we keep also source code information for display in verbose
  ## mode and help debugging.
  result = readChar(f)
  if result == '\n':
    ossSetDebug(ossRow, ossLine)
    ossCol = 0
    inc(ossRow)
    # Create an empty string of capacity 120 to store the current ossLine while
    # read character by character. A better strategy would be to read the whole
    # ossLine and index the current character though.
    ossLine = newStringOfCap(120)
  else:
    inc(ossCol)
    ossLine = ossLine & result


proc ossIdentifier(sym: var int32) =
  ## Parse an identifier.
  ## 
  ## ```
  ## ident = letter {letter | digit | "_"}.
  ## ```
  var i = 0
  ossId = newStringOfCap(ossIdLen)
  until ossCh < '0' or ossCh > '9' and ossCh < 'A' or ossCh > 'Z' and ossCh < 'a' or ossCh > 'z' and ossCh != '_':
    if ossCh == '_':
      discard
    elif i < ossIdLen:
      ossId = ossId & ossCh
      inc(i)
    else:
      ossMark("Identifier too long; truncated")
    ossCh = ossReadC(R)

  var k = 0
  while (k < len(keyTab)) and (ossId != keyTab[k].id):
    inc(k)
  if k < len(keyTab):
    sym = keyTab[k].sym
  else:
    sym = ossIdent


proc ossNumber(sym: var int32) =
  ## Parse a number.
  ## 
  ## ```
  ## integer = digit {digit | "_"}.
  ## ```
  ossVal = 0
  sym = ossInt
  until ossCh < '0' or ossCh > '9' and ossCh != '_':
    if ossCh == '_':
      discard
    elif ossVal <= (ossMaxInt - ord(ossCh) + ord('0')) div 10:
      ossVal = int32(10 * ossVal + (ord(ossCh) - ord('0')))
    else:
      ossMark("Number too large for int32: " & $(10 * ossVal + (ord(ossCh) - ord('0'))))
      ossVal = 0
    ossCh = ossReadC(R)


proc ossComment =
  ## Skip input while in comment. Comments can be included and that's the
  ## reason why this procedure is recursive.
  until ossCh == '#' or endOfFile(R):
    until ossCh == ']' or endOfFile(R):
      ossCh = ossReadC(R)
      while ossCh == '#' and not endOfFile(R):
        ossCh = ossReadC(R)
        if ossCh == '[':
          ossComment()
    until ossCh != ']' or endOfFile(R):
      ossCh = ossReadC(R)
  if not endOfFile(R):
    ossCh = ossReadC(R)
  else:
    ossMark("Multi-ossLine comment not terminated; end of file reached")


proc ossGet*(sym: var int32) =
  ## Get a new symbol from source code scanning. Its entry in the keywords
  ## table is returned into `ossSym`.
  # To prevent infinite loops while synchronizing after errors
  if sym == ossEof:
    ossMark("End of file reached")
    quit(QuitFailure)

  until sym != ossNull:
    # Skip spaces
    while not endOfFile(R) and (ossCh <= ' '):
      ossCh = ossReadC(R)

    if endOfFile(R):
      sym = ossEof
      return

    # Keep current identation level
    ossIndent = ossCol

    if ossCh < 'A':
      if ossCh < '0':
        case ossCh
        of '\'': # Characters
          ossCh = ossReadC(R)
          if ossCh == '\\': # TODO: Limited support presently: only '\0' .. '\9', '\t' and '\n'
            ossCh = ossReadC(R)
            case ossCh
            of '\'':
              ossVal = int32(ord(ossCh))
            of '0' .. '9':
              ossVal = int32(ord(ossCh)) - int32(ord('0'))
            of 't': # Tabulation
              ossVal = 9
            of 'n': # Line feed
              ossVal = 10
            else:
              ossMark("Character escape sequence not supported")
              ossVal = 0
          else:
            ossVal = int32(ord(ossCh))
          until ossCh == '\'' or endOfFile(R):
            ossCh = ossReadC(R)
          ossCh = ossReadC(R)
          sym = ossChar

        of '!':
          ossCh = ossReadC(R) 
          if ossCh == '=':
            ossCh = ossReadC(R)
            sym = ossNeq
          else:
            ossMark("Found !; user-defined operators are not supported")
            sym = ossNull
        of '#':
          ossCh = ossReadC(R) 
          if ossCh == '[': # Multi-ossLine comment
            sym = ossNull
            ossComment()
          else: # Single-ossLine comment
            while ossCh != '\n' and not endOfFile(R):
              ossCh = ossReadC(R)
            sym = ossNull
        of '(':
          ossCh = ossReadC(R)
          sym = ossLparen
        of ')':
          ossCh = ossReadC(R)
          sym = ossRparen
        of '*':
          ossCh = ossReadC(R)
          sym = ossTimes
        of '+':
          ossCh = ossReadC(R)
          sym = ossPlus
        of ',':
          ossCh = ossReadC(R)
          sym = ossComma
        of '-':
          ossCh = ossReadC(R)
          sym = ossMinus
        of '.':
          ossCh = ossReadC(R)
          if ossCh == '.':
            ossCh = ossReadC(R)
            if ossCh == '<':
              sym = ossRangeL
            else:
              sym = ossRange
          else:
            sym = ossPeriod
        of '/':
          ossCh = ossReadC(R)
          sym = ossNull
        else: # $ %
          sym = ossNull
          ossMark("Invalid character " & $ossCh)
          ossCh = ossReadC(R)

      elif ossCh < ':':  # Numbers
        ossNumber(sym)

      elif ossCh == ':':
        ossCh = ossReadC(R)
        sym = ossColon
      elif ossCh == '=':
        ossCh = ossReadC(R)
        if ossCh == '=':
          ossCh = ossReadC(R)
          sym = ossEql
        else:
          sym = ossBecomes
      elif ossCh == ';':
        ossCh = ossReadC(R)
        sym = ossSemicolon

      elif ossCh == '<': # Boolean operators
        ossCh = ossReadC(R)
        if ossCh == '=':
          ossCh = ossReadC(R)
          sym = ossLeq
        else:
          sym = ossLss

      elif ossCh == '=':
        ossCh = ossReadC(R)
        sym = ossEql
      elif ossCh == '>':
        ossCh = ossReadC(R)
        if ossCh == '=':
          ossCh = ossReadC(R)
          sym = ossGeq
        else:
          sym = ossGtr
      else: # ? @
        ossCh = ossReadC(R)
        ossMark("Invalid character " & $ossCh)
        sym = ossNull

    elif ossCh < '[':
      ossIdentifier(sym)

    elif ossCh < 'a':
      case ossCh
      of '[':
        sym = ossLbrak
      of ']':
        sym = ossRbrak
      else: #[  _ ` ^ ]#
        ossMark("Invalid character " & $ossCh)
        sym = ossNull
      ossCh = ossReadC(R)

    elif ossCh < '{':  # Identifier
      ossIdentifier(sym)

    else: # '{', '}', '|', '~'
      ossMark("Invalid character " & $ossCh)
      sym = ossNull
      ossCh = ossReadC(R)


proc ossInit*(f: File; pos: int; o: File) =
  ## Initialize scanner module global variables.
  R = f
  W = o
  ossError = false
  ossErrPos = pos
  ossCol = 0
  ossRow = 1
  ossIi = 0
  ossIndents[ossIi] = 1
  ossCh = ossReadC(R)
