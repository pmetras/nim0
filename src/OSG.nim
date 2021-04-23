## Nim0 language in Nim
## ====================
## The code generator.
## 
## Emit RISC machine instructions.

import OSS
import strutils, strformat

const
  osgWordSize* = 4      ## int32(sizeof(uint32)) / int32(sizeof(int32))

  osgMemSize* = 8192'i32## Memory size in bytes. Negative addresses are used to call compiler standard
                        ## procedures and functions (i.e. IO).

  ## Class / mode
  osgHead* = 0          ## The object defines a new scope of identifiers in a record/object or
                        ## in a procedure call [CC76].
  osgConst* = 1         ## The object is a contant / mode Const
  osgVar* = 2           ## The object is a variable / call by value procedure parameter [CC76].
  osgPar* = 3           ## The object is a call by `var` (address) procedure parameter in a procedure call [CC76].
  osgFld* = 4           ## The object is an object/record field
  osgTyp* = 5           ## The object is a type
  osgSProc* = 6         ## The object is a standard procedure (magic procedure included
                        ## in the system like readInt, writeInt) [CC80].
  osgSFunc* = 7         ## The object is a standard function name (magic system function
                        ## like ord, endOfInput) [CC80]
  osgProc* = 8          ## The object is a procedure name [CC76]
  osgNoTyp* = 9         ## The object has no type (Not used)
  osgReg = 10           ## The object is available in a register.
  osgRegI = 11          ## Addressing mode is Register Indirect [CC57]. Emerges from
                        ## evaluation of an indexed variable.
  osgCond = 12          ## Conditional mode [CC62]

  ## Reserved registers
  SB = 13               ## R13 (Static base) contains the address where the code has been loaded.
                        ## All global variable adresses are relative to this address.
                        ## And also top of the registers stack: R13 can't be used by the
                        ## expression evaluation stack!
  SP = 14               ## R14 (Stack pointer) [CC71]. Whenever the program/system is started,
                        ## R14 must be initialized to point to an area of memory reserved for the stack.
  LNK = 15              ## R15 (Link) = Return address when calling a subroutine [CC71]

  ## Form of symbol
  osgBoolean* = 0       ## Symbol is a boolean
  osgInteger* = 1       ## Symbol is an integer
  osgCharacter* = 2     ## Symbol is a character
  osgArray* = 3         ## Symbol is an array
  osgRecord* = 4        ## Symbol is an object/record

  ## Register instructions [CC47].
  osgMOV* = 0
  osgLSL* = 1
  osgASR* = 2
  osgROR* = 3
  osgAND* = 4
  osgANN* = 5
  osgIOR* = 6
  osgXOR* = 7
  osgADD* = 8
  osgSUB* = 9
  osgCMP* = 9
  osgMUL* = 10
  osgDIV* = 11

  ## Memory Load/Store instructions [CC48].
  osgLDW* = 0
  osgSTW* = 2

  ## Branch instructions [CC48]
  BR = 0        ## Branch through register
  #BLR = 1       ## Branch through register with link (return address) NOT USED IN NIM0
  BC = 2        ## Branch conditional [CC62]
  BL = 3        ## Branch and link (return address) [CC71]

  ## Conditions [C49] and [CC63].
  MI = 0        ## Negative (MInus) condition: `N` flag set
  PL = 8        ## Positive (PLus) condition: `N` flag not set
  EQ = 1        ## EQual condition `==`: `Z` flag set
  NE = 9        ## Not Equal condition `!=`: `Z` flag not set
  LT = 5        ## Less Than condition `<`: `N` flag different from `V` flag
  GE = 13       ## Greater or Equal condition `>=`: `N` flag equals `V` flag
  LE = 6        ## Less or Equal condition `<=`: `N` flag different from `V` flag or `Z` flag set
  GT = 14       ## Greater Than condition `>`: `N` flag equals `V` flag and `Z` flag not set
  AL = 7        ## ALways: True
  NV = 15       ## NeVer: False


  ## Standard procedures and functions
  osgStdOpenInput* = 0x000F_FFFF'u32     ## OpenInput (not required in Nim0)
  osgStdReadInt* = 0x000F_FFFE'u32       ## ReadInt from command line
  osgStdEot* = 0x000F_FFFD'u32           ## End of text is reached (no more parameters in command line)
  osgStdWriteInt* = 0x000F_FFFC'u32      ## Write integer to standard output
  osgStdWriteChar* = 0x000F_FFFB'u32     ## Write `chr(x)` to standard output
  osgStdWriteLine* = 0x000F_FFFA'u32     ## End line
  osgStdLED* = 0x000F_FFF9'u32           ## Show `x` on LED (not implemented)
  osgStdSwitch* = 0x000F_FFF8'u32        ## Read state of switches
  osgStdWriteBool* = 0x000F_FFF7'u32     ## Write a boolean value
  osgStdRead* = 0x000F_FFF6'u32          ## Generic read from standard input


type
  osgObject* = ref osgObjDesc   ## Symbol entry
  osgType* = ref osgTypeDesc    ## Type descriptor

  osgItem* = object             ## Item for delayed code generation [CC53]. Item objects are
                                ## short-lived contextual objects and are generaly allocated
                                ## on the stack: there's no need for `ref`. Items are returned
                                ## by the OSP parser procedures. You can understand an `osgItem` as
                                ## an instantiation of an `osgObject` in a context.
    mode*: int32                ## The addressing mode (direct, immediate or register) used
                                ## to access the symbol [CC53].
    lev*: int32                 ## Level in included contexts.
    typ*: osgType               ## The type of the local symbol [CC54].
    a*: int32                   ## Additionnal attributes [CC53]. Following attributes `a`, `b` and `r`
                                ## are used by Wirth to store many intermediate values depending on
                                ## context and documenting them is difficult. Here are some explanations.
                                ## The local value of the symbol if the item is a constant (`mode == Const`).
                                ## Or the address offset to access the field if the item is an `object` [CC57].
                                ## Value in memory at address `a`.
                                ## Or the index if the item is an `array` [CC57].
                                ## In case of `osgCond` mode, the fixup addresses (F-list) for `or` conditions [CC68].
                                ## In case of `Proc` mode, contains the current value of `osgPc`, whci is the
                                ## entry address of the procedure prologue [CC76].
    b: int32                    ## Another additional attribute like `a`.
                                ## In case of `osgCond` mode, the fixup addresses (T-list) for `and` conditions [CC68].
    r: int32                    ## The register number where a value is temporarily stored (cached) [CC53].
                                ## Instead of refering to memory, operations on register are
                                ## faster. Value held in register `R[r]`.
                                ## If the item is an `array`, register `R[r]` holds the index.
                                ## In case of `osgCond` mode, it contains which relation is specified by the
                                ## comparison [CC62].


  osgObjDesc* = object          ## Symbol descriptor in the table of symbols [CC39].
    class*: int32               ## It indicates whether the identifier denotes a constant,
                                ## a variable, a type or a procedure [CC39].
    lev*: int32                 ## Denotes the nesting level of the declared object [CC76].
    next*: osgObject
    dsc*: osgObject             ## Link between scopes of procedure [CC76].
    typ*: osgType               ## The symbol type [CC39]
    name*: string               ## Name of the symbol [CC39]
    val*: int32                 ## Value of the symbol, if known at compile time [CC39]
                                ## Or address of procedure in bytes.
    nofpar*: int32              ## Number of formal parameters when object is a procedure.
    comd*: bool                 ## The symbol is exported. In case of a `proc`, it means
                                ## that it can be called from the RISC emulator.

  osgTypeDesc* = object         ## Data types of symbols [CC40]. Named types are represented
                                ## in the symbol table by an entry of type `osgObjDesc`
                                ## which in turns refers to an element of type `osgTypeDesc`.
    form*: int32                ## Type form: bool/int/array/object. Differentiates between
                                ## elementary types (`bool`, `int`) and structured types
                                ## (`array`, `object` ) [CC40].
    dsc*: osgObject
    base*: osgType              ## Base type of an array element [CC41].
    size*: int32                ## Size of the type for memory allocation [CC42].
    len*: int32                 ## The number of elements in an array [CC41].
    nofpar*: int32              ## Number of formal parameters?

var
  osgBoolType*: osgType ## A boolean type singleton object
  osgIntType*: osgType  ## An integer type singleton object
  osgCharType*: osgType ## A character type singleton object
  #osgCurlev*: int32     ## Current level in procedures declarations NOT USED IN NIM0
  osgPc*: int32         ## Program Counter
  curSB: int32          ## The current static base, when using code relocation (not the
                        ## case in Nim0).
  entry: int32          ## The module entry point, used to initialize static data and
                        ## global variables.
  fixlist: int32        ## List of addresses of BL instructions that need to be fixed up
                        ## by the linker when loading the resulting object code [CC96] and [PO81].
  fixorgD: int32        ## List of addresses of LDR instructions that need to be fixed up
                        ## by the linker when loading the resulting object code [CC96] and [PO81].
  RH: int32             ## Register stack pointer [CC51]. The registers are used
                        ## as a stack from R0, R1, R2, etc. to evaluate expressions.

  W: File = stdout
  relmap: array[6, int32]
  osgCode*: array[osgMemSize, uint32] ## The resulting code
  mnemo0, mnemo1: array[16, string]   ## For decoder, to print RISC assembler mnemonics

# Type singletons
osgBoolType = osgType(form: osgBoolean, size: osgWordSize)
osgIntType = osgType(form: osgInteger, size: osgWordSize)
osgCharType = osgType(form: osgCharacter, size: osgWordSize)

relmap[0] = EQ
relmap[1] = NE
relmap[2] = LT
relmap[3] = LE
relmap[4] = GT
relmap[5] = GE

mnemo0[osgMOV] = "MOV"
mnemo0[osgLSL] = "LSL"
mnemo0[osgASR] = "ASR"
mnemo0[osgROR] = "ROR"
mnemo0[osgAND] = "AND"
mnemo0[osgANN] = "ANN"
mnemo0[osgIOR] = "IOR"
mnemo0[osgXOR] = "XOR"
mnemo0[osgADD] = "ADD"
mnemo0[osgSUB] = "SUB"
mnemo0[osgMUL] = "MUL"
mnemo0[osgDIV] = "DIV"

mnemo1[PL] = "PL"
mnemo1[MI] = "MI"
mnemo1[EQ] = "EQ"
mnemo1[NE] = "NE"
mnemo1[LT] = "LT"
mnemo1[GE] = "GE"
mnemo1[LE] = "LE"
mnemo1[GT] = "GT"
mnemo1[NV] = "NO"


#[ -------------------- output ----------------------- ]#

proc osgWriteReg*(r: uint32; f: File = W) =
  ## Write an anonymous or well-know register name.
  if r < 13:
    write(f, "R")
    write(f, r)
  elif r == SB:
    write(f, "SB")
  elif r == SP:
    write(f, "SP")
  elif r == LNK:
    write(f, "LNK")


proc osgWriteDebugSrc(pc: int32; j: var int32) =
  ## Write the Nim0 source lines corresponding to the RISC assembler
  ## line `pc`.
  while (ossDebug[j].pc == pc or ossDebug[j].pc == -1) and ossDebug[j].row > 0:
    write(W, &"\t\t\t\t\t# {ossDebug[j].row:>03} ")
    writeLine(W, ossDebug[j].line)
    inc(j)


proc osgWriteDebugAsm*(i: int32; code: openArray[uint32] = osgCode; f: File = W) =
  ## Write the RISC assembler corresponding to the memory location `i` in
  ## code. The procedure signature has been changed to be able to use it
  ## from the RISC emulator.
  let w = code[i]
  let a = (w and 0x0F00_0000'u32) shr 24
  let b = (w and 0x00F0_0000'u32) shr 20

  write(f, &"{i:>04} ") # Memory 
  write(f, toHex(w))    # Raw hexadecimal
  write(f, '\t')

  # Register instruction
  if (w and 0x8000_0000'u32) == 0: # Bit 31 is set
    let op = (w and 0x000F_0000'u32) shr 16
    # MOV instruction has only 2 operands.
    # And special case: if instruction is SUB and next one is a branch
    # we use the CMP mnemonic for prettier asm source, to show that's
    # a branch comparison.
    if op == osgMOV or (op == osgSUB and ((code[i + 1] and 0x8000_0000'u32) != 0) and ((code[i + 1] and 0x4000_0000'u32) != 0)):
      if op == osgSUB:
        write(f, "CMP")
      else:
        write(f, mnemo0[op])
      write(f, " ")
      osgWriteReg(a, f)
      write(f, ", ")
      if (w and 0x4000_0000'u32) == 0: #[ ~q ]#
        osgWriteReg(w and 0x0000_000F'u32, f)
      else:
        var c = w and 0x0000_FFFF'u32
        if (w and 0x1000_0000'u32) != 0: #[ v ]#
          c = c or 0xFFFF_0000'u32
        write(f, c)
    else:
      write(f, mnemo0[op])
      write(f, " ")
      osgWriteReg(a, f)
      write(f, ", ")
      osgWriteReg(b, f)
      if (w and 0x4000_0000'u32) == 0: #[ ~q ]#
        write(f, ", ")
        osgWriteReg(w and 0x0000_000F'u32, f)
      else:
        var c = w and 0x0000_FFFF'u32
        if (w and 0x1000_0000'u32) != 0: #[ v ]#
          c = c or 0xFFFF_0000'u32
        write(f, ", ", c)

  # Load/store instruction
  elif (w and 0xC000_0000'u32) == 0x8000_0000'u32:
    let c = w and 0x000F_FFFF'u32
    case c
    of osgStdReadInt:
      write(f, "readInt -> ")
      osgWriteReg(a, f)
    of osgStdEot:
      write(f, "endOfText -> ")
      osgWriteReg(a, f)
    of osgStdWriteInt:
      write(f, "writeInt <- ")
      osgWriteReg(a, f)
    of osgStdWriteChar:
      write(f, "writeChar <- ")
      osgWriteReg(a, f)
    of osgStdWriteLine:
      write(f, "writeLine")
    of osgStdOpenInput:
      write(f, "openInput (NOP)")
    of osgStdLED:
      write(f, "LED (NOP")
    of osgStdSwitch:
      write(f, "readSwitch (NOP")
    of osgStdWriteBool:
      write(f, "writeBool <- ")
      osgWriteReg(a, f)
    else:
      if (w and 0x2000_0000'u32) != 0: # u
        write(f, "STW")
      else:
        write(f, "LDW")
      write(f, " ")
      osgWriteReg(a, f)
      write(f, ", ")
      osgWriteReg(b, f)
      write(f, ", ", c)
      
  # Branch instruction
  else:
    write(f, "B")       # Branch...
    if (w and 0x1000_0000'u32) != 0: # v == 1
      write(f, "L")     # ... with Link (return address)
    write(f, mnemo1[a]) # Condition
    write(f, " ")
    if (w and 0x2000_0000'u32) == 0: # u == 0
      osgWriteReg(w and 0x0000_000F'u32, f)
    else:
      var off = cast[int32](w and 0x00FF_FFFF'u32)
      if off >= 0x0080_0000'i32:
        off = off - 0x0100_0000'i32
      write(f, off)


proc osgDecode* =
  ## Dump RISC object memory content.
  var i, j: int32
  writeLine(W, toHex(osgCode[0]), toHex(osgCode[1]))
  i = 0
  j = 0
  while i < osgPc:
    # Print source code debug information
    osgWriteDebugSrc(i, j)

    # Print corresponding assembler instructions
    osgWriteDebugAsm(i)
    writeLine(W, "")
    inc(i)
  writeLine(W, "")


proc osgDebugCode*(msg: string = "Code is now") =
  ## Can be called when debugging compiler generation to dump
  ## the content of the last instructions in `osgCode` memory.
  ## The header message is printed before the memory dump.
  echo msg
  var i: int32 = max(osgPc - 11, 0) # Show 10 lines of osgCode
  var j: int32 = 0
  while i <= osgPc - 1:
    # Print source code debug information
    osgWriteDebugSrc(i, j)

    # Print corresponding assembler instructions
    osgWriteDebugAsm(i)
    inc(i)
    writeLine(W, "")
  writeLine(W, "")


# Emit RISC instructions
# ----------------------
proc osgPut0(op, a, b, c: int32) =
  ## Emit format-0 register instruction [CC47] and [CC52].
  ## Deposit an instruction into the `osgCode` global array using the variable
  ## `osgPc` as index denoting the next free location.
  ## Contrarily to Wirth, we don't use Honer factorization to show the various
  ## parts of the instruction. We also keep a link to the Nim0 source code for
  ## display in the resulting assembler code.
  ossSetDebugPc(osgPc)
  #osgCode[osgPc] = ((a * 0x10 + b) * 0x10 + op) * 0x1_0000 + c
  osgCode[osgPc] = cast[uint32]((a shl 24) or (b shl 20) or (op shl 16) or (c and 0x0000_000F'i32))
  inc(osgPc)


proc osgPut1(op, a, b, im: int32) =
  ## Emit format-1 register instruction [CC47] and [CC52].
  ## Deposit an instruction into the `osgCode` global array using the variable
  ## `osgPc` as index denoting the next free location.
  ## Contrarily to Wirth, we don't use Honer factorization to show the various
  ## parts of the instruction. We also keep a link to the Nim0 source code for
  ## display in the resulting assembler code.
  ossSetDebugPc(osgPc)
  var v = op
  if im < 0:
    v = op + 0x1000  #[ set v-bit ]#
  #osgCode[osgPc] = (((a + 0x40) * 0x10 + b) * 0x10 + v) * 0x1_0000 + (im mod 0x1_0000)
  osgCode[osgPc] = cast[uint32](((a + 0x40) shl 24) or (b shl 20) or (v shl 16) or (im and 0x0000_FFFF'i32))
  inc(osgPc)


proc osgPut2(op, a, b, off: int32) =
  ## Emit load/store instruction [CC48] and [CC53].
  ## Deposit an instruction into the `osgCode` global array using the variable
  ## `osgPc` as index denoting the next free location.
  ## Contrarily to Wirth, we don't use Honer factorization to show the various
  ## parts of the instruction. We also keep a link to the Nim0 source code for
  ## display in the resulting assembler code.
  ossSetDebugPc(osgPc)
  #osgCode[osgPc] = (((op + 8) * 0x10 + a) * 0x10 + b) * 0x10_0000 + (off mod 0x1_0000)
  osgCode[osgPc] = cast[uint32](((op + 8) shl 28) or (a shl 24) or (b shl 20) or (off and 0x000F_FFFF'i32))
  inc(osgPc)


proc osgPut3(op, cond, off: int32) =
  ## Emit branch instruction [CC49] and [CC53].
  ## Deposit an instruction into the `osgCode` global array using the variable
  ## `osgPc` as index denoting the next free location.
  ## Contrarily to Wirth, we don't use Honer factorization to show the various
  ## parts of the instruction. We also keep a link to the Nim0 source code for
  ## display in the resulting assembler code.
  ossSetDebugPc(osgPc)
  #osgCode[osgPc] = ((op + 12) * 0x10 + cond) * 0x100_0000 + (off mod 0x100_0000)
  osgCode[osgPc] = cast[uint32](((op + 12) shl 28) or (cond shl 24) or (off and 0x00FF_FFFF'i32))
  inc(osgPc)


proc osgIncR =
  ## Increment the register number used as registers stack top.
  if RH < SB:
    inc(RH)
  else:
    ossMark("Compiler error: register stack overflow")


proc osgCheckRegs* =
  ## After unstacking the registers stack, check that we don't underflow.
  if RH != 0:
    ossMark("Compiler error: register stack underflow")
    RH = 0


proc osgSetCC(x: var osgItem; n: int32) =
  ## Set the comparison condition of a `osgItem`, `n` containing the type
  ## of relation [CC63].
  x.mode = osgCond
  x.a = 0
  x.b = 0
  x.r = n


proc osgNegated(cond: int32): int32 =
  ## Find the osgNegated condition value of the `cond` parameter [CC63].
  ## For instance, `EQ` (`==`) with value 1 has `NE` (`!=`) with value 9.
  if cond < 8:
    result = cond + 8
  else:
    result = cond - 8


proc osgInvalSB =
  ## Invalidate the current static base
  curSB = 1


proc osgFix(at, with: int32) =
  ## Fixup [CC63]. Create a temporary instruction for a branch location that is
  ## not known yet. When the code is emitted and the final location is known,
  ## the instruction is replaced with the definitive address.
  #osgCode[at] = (osgCode[at] div 0x100_0000) * 0x100_0000 + (with mod 0x100_0000)
  osgCode[at] = (osgCode[at] and 0xFF00_0000'u32) or (cast[uint32](with) and 0x00FF_FFFF'u32)


proc osgFixLink*(L: int32) =
  ## Fix temporary branches locations replacing them with the final adresses.
  ## The `L` parameter is the first of a chain. The next value in the chain has
  ## been temporarily stored in the offset value `off` of the branch instruction.
  ## Note that branch instruction use addresses relative to the instruction
  ## location (PC-relative) therefore the value `osgPc - L - 1` is used to fix it.
  ## Note also that this procedure, like many others in Wirth's code, use global
  ## variables, in that case `osgPc`, and is very dependant of the location in
  ## the code where it is called.
  var v = L
  while v != 0:
    if v < osgMemSize:
      let L1 = cast[int32](osgCode[v] and 0x00FF_FFFF'u32)
      osgFix(v, osgPc - v - 1)
      v = L1
    else:
      ossMark("Compiler error: can't fix address to location "  & $v & " outside of memory")
      quit(QuitFailure)


proc GetSB =
  ## Get the static base address used to access global variables.
  ## 
  ## Because we use a simpler memory model (fixed adresses) than Wirth's,
  ## we don't need to relocate the code and we can assume that `SB` is
  ## located at begining of memory.
  # We keep Wirth's relocation code in case you want to complete the linker phase.
  #if curSB == 1:
  #  osgPut2(osgLDW, SB, 0, osgPc - fixorgD)
  #  fixorgD = osgPc - 1
  #  curSB = 0
  curSB = 0


proc osgLoad(x: var osgItem) =
  ## Load a variable into a register with constant folding [CC55].
  ## If the compiler is able to do the operation at compilation time, when the
  ## argument is a constant or a local or global variable, we use the best
  ## instructions.
  ## `x.r` now contains the register where the variable has been loaded.
  ## 
  ## Loading a boolean variable should not be a frequent case and is not
  ## optimized in order to have better code for boolean assigments [CC68].
  # If the items is not already in a register, load it
  if x.mode != osgReg:
    if x.mode == osgVar:  # Procedure parameter (call by value)
      if x.lev == 0:
        GetSB()   # Global variable relative to static base
      osgPut2(osgLDW, RH, x.r, x.a)
      x.r = RH
      osgIncR()
    elif x.mode == osgPar: # Procedure parameter (call by var)
      osgPut2(osgLDW, RH, x.r, x.a)
      osgPut2(osgLDW, RH, RH, 0)
      x.r = RH
      osgIncR()
    elif x.mode == osgConst: # Constant value
      if (x.a >= 0x1_0000) or (x.a < -0x1_0000):  # Why not 32 bits?
        ossMark("Constant " & $x.a & " is too large")
      osgPut1(osgMOV, RH, 0, x.a)
      x.r = RH
      osgIncR()
    elif x.mode == osgRegI: # Register indirect for array item or object field
      osgPut2(osgLDW, x.r, x.r, x.a)
    elif x.mode == osgCond: # Load of boolean value
      osgPut3(BC, osgNegated(x.r), 2)
      osgFixLink(x.b)
      osgPut1(osgMOV, RH, 0, 1)
      osgPut3(BC, AL, 1)
      osgFixLink(x.a)
      osgPut1(osgMOV, RH, 0, 0)
      x.r = RH
      osgIncR()
    x.mode = osgReg # Now item `x` ìs accessed in register


proc osgLoadAddr(x: var osgItem) = 
  ## Emit the instructions to load a variable by address (VAR) in the register
  ## in a procedure call [CC78].
  ## Contrarily to `osgLoad`, we load the variable into a register
  ## even if it already done (see osgLoad)?
  if x.mode == osgVar:
    if x.lev == 0:  # Global variable relative to static base
      GetSB()
    osgPut1(osgADD, RH, x.r, x.a)
    x.r = RH
    osgIncR()
  elif x.mode == osgPar: # Procedure parameter, use SP
    if x.lev == 0:  # Global variable relative to static base
      GetSB()
    osgPut2(osgLDW, RH, x.r, x.a)
    x.r = RH
    osgPut1(osgADD, RH, RH, x.b)
    osgIncR()
  elif (x.mode == osgRegI) and (x.a != 0): # Register indirect for array item or object field
    osgPut1(osgADD, x.r, x.r, x.a)
  else:
    ossMark("Can't take address from value")
  x.mode = osgReg # Now the item `x` is accessible in register


proc osgLoadCond(x: var osgItem) =
  ## Converts `osgItem` `x` into `osgCond` mode, for later boolean evaluation [CC67].
  ## `false` is represented by 0.
  if x.typ.form == osgBoolean:
    if x.mode == osgConst:
      x.r = 15 - x.a * 8
    else:
      osgLoad(x)
      osgPut1(osgCMP, x.r, x.r, 0)
      x.r = NE
      dec(RH)
    x.mode = osgCond
    x.a = 0
    x.b = 0
  else:
    ossMark(ossId & " is not boolean")


proc osgMerged(L0, L1: int32): int32 =
  ## Merge fixup lists (T-list or F-lists) when generating boolean conditions
  ## when a list of a subordinate expression may merge with the list of its
  ## containing expression [CC68]. It yields as its value the concatenation of
  ## its argument lists.
  var L2, L3: int32
  var v = L1
  if L0 != 0:
    L3 = L0
    until L3 == 0:
      L2 = L3
      # TODO BUG with Nim0 code like:
      # if i == 1 and j == 2:
      # Must be written: if (i == 1) and (j == 2):
      L3 = cast[int32](osgCode[L2] and 0x00FF_FFFF)
    osgCode[L2] = osgCode[L2] or cast[uint32](v)
    v = L0
  return v


#[ ----------------------------------------------- ]#

proc osgMakeConstItem*(x: var osgItem; typ: osgType; val: int32) =
  ## Create a constant `osgItem` of type `typ` from the given value `val`.
  x.mode = osgConst
  x.typ = typ
  x.a = val


proc osgMakeItem*(x: var osgItem; y: osgObject; curlev: int32) =
  ## Convert `y` `osgObject` into a corresponding `osgItem` returned as `x`
  ## [CC77]. Difference between the addressing of local and global variables
  ## should be taken into account. The handling of intermediate-level variables
  ## is not treated here.
  x.mode = y.class
  x.typ = y.typ
  x.a = y.val
  if y.lev == 0: # Global variable is SB based
    x.r = SB
  elif y.lev == curlev: # Local variable is SP based
    x.r = SP
    #x.r = y.lev
  else:
    ossMark("Compiler error: level")
    x.r = 0
  if y.class == osgPar:
    x.b = 0
  if (y.lev > 0) and (y.lev != curlev) and (y.class != osgConst):
    ossMark("Compiler error: level error")


proc osgField*(x: var osgItem; y: osgObject) =
  ## Calculate the address of the field named `y.name` in the object `x` [CC57].
  #[  x = x.y  ]#
  if (x.mode == osgVar) or (x.mode == osgRegI):
    x.a = x.a + y.val
  elif x.mode == osgPar:
    osgPut2(osgLDW, RH, x.r, x.a)
    x.mode = osgRegI
    x.r = RH
    x.a = y.val
    osgIncR()


proc osgIndex*(x, y: var osgItem) =
  ## Calculate the address of an indexed element of an array and generate the
  ## code to access it [CC57].
  #[  x = x[y]  ]#
  var s: int32
  if y.mode == osgConst:
    if (y.a < 0) or (y.a >= x.typ.len):
      ossMark("Index " & $y.a & " of array " & ossId & " out of bounds [0 .. " & $x.typ.len & "]")
    if x.mode == osgPar:
      osgPut2(osgLDW, RH, x.r, x.a)
      x.mode = osgRegI   # Register indirect
      x.a = 0
    x.a = x.a + y.a * x.typ.base.size
  else:
    s = x.typ.base.size
    if y.mode != osgReg:
      osgLoad(y)
    # There is no run-time check for index out-of-bounds in Nim0 because
    # trap instructions are not supported by RISC emulator [CC59] and [PO50].
    # osgPut1(osgCMP, 0, y.r, x.type.base.len)
    # osgPut3(osgBL, HI, trap - osgPc)
    if s == 4:  # Optimization: binary left shift by 2 instead of multiply by 4.
      osgPut1(osgLSL, y.r, y.r, 2)
    else:
      osgPut1(osgMUL, y.r, y.r, s)
    if x.mode == osgVar:
      if x.lev == 0: # Global variables are SB based
        GetSB()
      osgPut0(osgADD, y.r, x.r, y.r)
      x.mode = osgRegI   # Register indirect
      x.r = y.r
    elif x.mode == osgPar:
      osgPut2(osgLDW, RH, SP, x.a)
      osgPut0(osgADD, y.r, RH, y.r)
      x.mode = osgRegI
      x.r = y.r
    elif x.mode == osgRegI:
      osgPut0(osgADD, x.r, x.r, y.r)
      dec(RH)


#[  Code generation for Boolean operators  ]#

proc osgNot*(x: var osgItem) =
  ## Negation of a boolean condition.
  #[  x = ~x  ]#
  var t: int32
  if x.mode != osgCond:
    osgLoadCond(x)
  x.r = osgNegated(x.r)
  t = x.a
  x.a = x.b
  x.b = t


proc osgAnd1*(x: var osgItem) =
  ## In a boolean `a and b` condition, a branch instruction must be emmitted
  ## before the second operand is processed, and this instruction address must
  ## be fixed up [CC67]. The address that is fixed up is in `x.b` (see `osgOr1`).
  #[  x = x and  ]#
  if x.mode != osgCond:
    osgLoadCond(x)
  osgPut3(BC, osgNegated(x.r), x.a)
  x.a = osgPc - 1
  osgFixLink(x.b)
  x.b = 0


proc osgAnd2*(x, y: var osgItem) =
  ## This is the second part of processing a boolean `a and b` instruction,
  ## evaluating `b` only if `a` is true [CC67].
  if y.mode != osgCond:
    osgLoadCond(y)
  x.a = osgMerged(y.a, x.a)
  x.b = y.b
  x.r = y.r


proc osgOr1*(x: var osgItem) =
  ## When parsing `a or b` condition, the branch instruction must be executed
  ## when the respective conditions are satisfied [CC67]. The adress tha is
  ## fixed up is in `x.a` (see `osgAnd1`).
  #[  x = x or  ]#
  if x.mode != osgCond:
    osgLoadCond(x)
  osgPut3(BC, x.r, x.b)
  x.b = osgPc - 1
  osgFixLink(x.a)
  x.a = 0


proc osgOr2*(x, y: var osgItem) =
  ## This is the second part of processin a boolean `a or b` instruction,
  ## lazily evaluating `b` only if `a` is false [CC67].
  if y.mode != osgCond:
    osgLoadCond(y)
  x.a = y.a
  x.b = osgMerged(y.b, x.b)
  x.r = y.r


#[  Code generation for arithmetic operators  ]#

proc osgNeg*(x: var osgItem) =
  ## Constant folding for number negation [CC55].
  ## If the argument is a constant, we execute the negation immediately.
  #[  x = -x  ]#
  if x.mode == osgConst:
    x.a = -x.a
  else:
    osgLoad(x)
    osgPut1(osgMOV, RH, 0, 0)
    osgPut0(osgSUB, x.r, RH, x.r)


proc osgAddOp*(op: int32; x, y: var osgItem) =
  ## Constant folding in addition / substraction [CC55].
  ## If the compiler is able to do the operation at compilation time, when one or both
  ## arguments are constants, we do it instead of generating the code.
  #[  x = x +- y  ]#
  if op == ossPlus:
    if (x.mode == osgConst) and (y.mode == osgConst):
      if x.a >= 0 and y.a > high(int32) - x.a:
        ossMark("Overflow " & $x.a & " + " & $y.a)
      elif x.a < 0 and y.a < low(int32) - x.a:
        ossMark("Underflow " & $x.a & " + " & $y.a)
      x.a = x.a + y.a
    elif y.mode == osgConst:
      osgLoad(x)
      if y.a != 0:
        osgPut1(osgADD, x.r, x.r, y.a)
    else:
      osgLoad(x)
      osgLoad(y)
      osgPut0(osgADD, RH - 2, x.r, y.r)
      dec(RH)
      x.r = RH - 1
  else: #[ op = ossMinus ]#
    if (x.mode == osgConst) and (y.mode == osgConst):
      x.a = x.a - y.a
    elif y.mode == osgConst:
      osgLoad(x)
      if y.a != 0:
        osgPut1(osgSUB, x.r, x.r, y.a)
    else:
      osgLoad(x)
      osgLoad(y)
      osgPut0(osgSUB, RH - 2, x.r, y.r)
      dec(RH)
      x.r = RH - 1


proc osgMulOp*(x, y: var osgItem) =
  ## Constant folding in multiplication [CC55].
  ## If the compiler is able to do the operation at compilation time, when one or both
  ## arguments are constants, we do it instead of generating the code.
  ## If one of the arguments is 2, we do a left shift instead.
  #[  x = x * y  ]#
  if (x.mode == osgConst) and (y.mode == osgConst):
    let o = x.a * y.a
    if (x.a != 0 and o div x.a != y.a) or (y.a != 0 and o div y.a != x.a):
      ossMark("Overflow " & $x.a & " * " & $y.a)
    x.a = x.a * y.a
  elif (y.mode == osgConst) and (y.a == 2):
    osgLoad(x)
    osgPut1(osgLSL, x.r, x.r, 1)
  elif y.mode == osgConst:
    osgLoad(x)
    osgPut1(osgMUL, x.r, x.r, y.a)
  elif x.mode == osgConst:
    osgLoad(y)
    osgPut1(osgMUL, y.r, y.r, x.a)
    x.mode = osgReg
    x.r = y.r
  else:
    osgLoad(x)
    osgLoad(y)
    osgPut0(osgMUL, RH - 2, x.r, y.r)
    dec(RH)
    x.r = RH - 1


proc osgDivOp*(op: int32; x, y: var osgItem) =
  ## Constant folding in division and modulo [CC56].
  ## If the compiler is able to do the operation at compilation time, when one or both
  ## arguments are constants, we do it instead of generating the code.
  ## If one of the arguments is 2, we do a right shift instead. Also, we can check that
  ## we don't divide by 0.
  #[  x = x op y  ]#
  if op == ossDiv:
    if (x.mode == osgConst) and (y.mode == osgConst):
      if y.a > 0:
        x.a = x.a div y.a
      else:
        ossMark("Bad divisor " & $y.a)
    elif (y.mode == osgConst) and (y.a == 2):
      osgLoad(x)
      osgPut1(osgASR, x.r, x.r, 1)
    elif y.mode == osgConst:
      if y.a > 0:
        osgLoad(x)
        osgPut1(osgDIV, x.r, x.r, y.a)
      else:
        ossMark("Bad divisor " & $y.a)
    else:
      osgLoad(y)
      osgLoad(x)
      osgPut0(osgDIV, RH - 2, x.r, y.r)
      dec(RH)
      x.r = RH - 1
  else: #[ op = ossMod ]#
    const U = 0x2000 # constant used in modulo
    if (x.mode == osgConst) and (y.mode == osgConst):
      if y.a > 0:
        x.a = x.a mod y.a
      else:
        ossMark("Bad modulus " & $y.a)
    elif (y.mode == osgConst) and (y.a == 2):
      osgLoad(x)
      osgPut1(osgAND, x.r, x.r, 1)
    elif y.mode == osgConst:
      if y.a > 0:
        osgLoad(x)
        osgPut1(osgDIV, x.r, x.r, y.a)
        osgPut0(osgMOV + U, x.r, 0, 0)
      else:
        ossMark("Bad modulus " & $y.a)
    else:
      osgLoad(y)
      osgLoad(x)
      osgPut0(osgDIV, RH - 2, x.r, y.r)
      osgPut0(osgMOV + U, RH - 2, 0, 0)
      dec(RH)
      x.r = RH - 1


proc osgRelation*(op: int32; x, y: var osgItem) =
  ## Generate the code for a comparison expression [CC63].
  #[  x = x ? y  ]#
  if y.mode == osgConst:
    osgLoad(x)
    osgPut1(osgCMP, x.r, x.r, y.a)
    dec(RH)
  else:
    osgLoad(x)
    osgLoad(y)
    osgPut0(osgCMP, x.r, x.r, y.r)
    dec(RH, 2)
  osgSetCC(x, relmap[op - ossEql])


proc osgStore*(x, y: var osgItem) =
  ## Code generation for assignment of variable `y` into `x` [CC69]. There is
  ## no special processing depending on the variable type as it is done in
  ## `osgLoad` procedure.
  #[  x = y  ]#
  osgLoad(y)
  if x.mode == osgVar: # Local variable or procedure parameter by var
    if x.lev == 0:  # Global variable are SB based
      GetSB()
    osgPut2(osgSTW, y.r, x.r, x.a)
  elif x.mode == osgPar: # Procedure parameter by value
    if x.lev == 0:  # Global variable are SB based
      GetSB()
    osgPut2(osgLDW, RH, x.r, x.a)
    osgPut2(osgSTW, y.r, RH, x.b)
  elif x.mode == osgRegI: # Array item or object field access
    osgPut2(osgSTW, y.r, x.r, x.a)
    dec(RH)
  else:
    ossMark("Illegal assignment")
  dec(RH)


proc osgVarParam*(x: var osgItem; ftype: osgType) =
  ## Code generation for a procedure call by-`var` parameter [CC78].
  var xmd = x.mode
  osgLoadAddr(x)
  if (ftype.form == osgArray) and (ftype.len < 0): #[ open array ]#
    if x.typ.len >= 0:
      osgPut1(osgMOV, RH, 0, x.typ.len)
    else:
      osgPut2(osgLDW, RH, SP, x.a + osgWordSize)
    osgIncR()
  elif ftype.form == osgRecord:
    if xmd == osgPar:
      osgPut2(osgLDW, RH, SP, x.a + osgWordSize)
      osgIncR()


proc osgValueParam*(x: var osgItem) =
  ## Code generation for a procedure call by-value parameter [CC78].
  osgLoad(x)


proc osgOpenArrayParam*(x: var osgItem) =
  ## Code generation for an `openarray` parameter in a procedure call [CC85].
  osgLoadAddr(x)
  if x.typ.len >= 0:
    osgPut1(osgMOV, RH, 0, x.typ.len)
  else:
    osgPut2(osgLDW, RH, SP, x.a + osgWordSize)
  osgIncR()


#[ --------------------------------- ]#

proc osgCFJump*(x: var osgItem) =
  ## Conditional forward jump [CC63]. It generates the necessary branch
  ## instruction according to its parameter `x.r` in such a way that the jump
  ## is taken if the specified condition is not satisfied. The destination
  ## location of branches is still unknown when the instruction is to be emitted.
  ## The problem is solved by a fixup: the location of the branch instruction is
  ## added as attribute `x.a` to the generated `osgItem`. This attribute is used
  ## later when the destination of the jump is known in order to complete the
  ## branch instruction with its true address.
  if x.mode != osgCond:
    osgLoadCond(x)
  osgPut3(BC, osgNegated(x.r), x.a)
  osgFixLink(x.b)     # Why a fixup in a jump instruction?
  x.a = osgPc - 1


proc osgFJump*(L: var int32) =
  ## Unconditional forward jump [CC64]. The address of the jump `L` is updated
  ## with `osgPc - 1` in order to create a chain of addresses that will be
  ## updated by the `osgFixLink` procedure when the final addresses are known.
  ## Note that as the RISC processor does not have an uncondition jump
  ## instruction, we use the conditional jump `BC` instruction with the
  ## always true `AL` condition.
  osgPut3(BC, AL, L)
  L = osgPc - 1


proc osgCBJump*(x: var osgItem; L: int32) =
  ## Conditional backward jump [CC65].  It generates the necessary branch
  ## instruction according to its parameter `x.r` in such a way that the jump
  ## is taken if the specified condition is not satisfied.
  if x.mode != osgCond:
    osgLoadCond(x)
  osgPut3(BC, osgNegated(x.r), L - osgPc - 1)


proc osgBJump*(L: int32) =
  ## Unconditional backward jump [CC65].
  ## Note that as the RISC processor does not have an uncondition jump
  ## instruction, we use the conditional jump `BC` instruction with the
  ## always true `AL` condition.
  osgPut3(BC, AL, L - osgPc - 1)


proc osgCall*(obj: var osgObject) =
  ## Emit the call to a procedure [CC78]. We use a branch and link `BL` to
  ## come back at procedure completion with the always true `AL` condition.
  osgPut3(BL, AL, (obj.val div osgWordSize) - osgPc - 1)
  RH = 0 # The registers stack is reset to 0


proc osgEnter*(parblksize, locblksize: int32; comd: bool) =
  ## Emit the instructions for a procedure prologue [CC71] and [CC77].
  var a: int32 = osgWordSize
  var r: int32 = 0
  osgPut1(osgSUB, SP, SP, locblksize)
  osgPut2(osgSTW, LNK, SP, 0)
  while a < parblksize:
    osgPut2(osgSTW, r, SP, a)
    inc(r)
    inc(a, osgWordSize)
  #[  if comd: osgPut2(Ldw, SB, 0, 0)  ]#


proc osgReturn*(size: int32) =
  ## Emit the instructions for a procedure epilogue [CC71] and [CC77].
  ## Unstack all local déclarations and restore return value.
  osgPut2(osgLDW, LNK, SP, 0)
  osgPut1(osgADD, SP, SP, size)
  osgPut3(BR, AL, LNK)
  RH = 0


proc osgOrd*(x: var osgItem) =
  ## Standard function to get the ordinal value of `x` [CC80]. The result
  ## is an integer. Makes sense only on `bool` in Nim0.
  osgLoad(x)
  x.typ = osgIntType


proc osgChr*(x: var osgItem) =
  ## Standard function to convert `int` value `x` to `char`. The result is
  ## a character.
  osgLoad(x)
  x.typ = osgCharType


proc osgOpenInput* =
  ## Standard procedure to start reading from command parameters [CC80].
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  ## 
  ## **NOT IMPLEMENTED IN NIM0**
  osgPut2(osgSTW, RH, RH, cast[int32](osgStdOpenInput))
  fixlist = osgPc - 1
  osgInvalSB()


proc osgReadInt*(x: var osgItem) =
  ## Standard procedure that reads an `int` from the parameters of the
  ## command called [CC80]. Returns its value as an integer in `RH` register.
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  osgLoadAddr(x)
  osgPut2(osgLDW, RH, RH, cast[int32](osgStdReadInt))
  osgPut2(osgSTW, RH, x.r, 0)
  dec(RH)


proc osgEot*(x: var osgItem) =
  ## Standard function to check that we are at end of input after reading
  ## the parameters of the command called [CC80]. Returns the fact that there
  ## is no more input parameter as
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  osgPut2(osgLDW, RH, RH, cast[int32](osgStdEot))
  fixlist = osgPc - 1
  osgPut1(osgCMP, x.r, RH, 0)
  osgSetCC(x, NE)
  osgInvalSB()


proc osgWriteBool*(x: var osgItem) =
  ## Standard procedure to output a boolean value.
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  osgLoad(x)
  osgPut2(osgSTW, x.r, x.r, cast[int32](osgStdWriteBool))
  fixlist = osgPc - 1
  dec(RH)
  osgInvalSB()


proc osgWriteChar*(x: var osgItem) =
  ## Standard procedure to output a character [CC80].
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  osgLoad(x)
  osgPut2(osgSTW, x.r, x.r, cast[int32](osgStdWriteChar))
  fixlist = osgPc - 1
  dec(RH)
  osgInvalSB()


proc osgWriteInt*(x: var osgItem) =
  ## Standard procedure to output an integer value [CC80].
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  osgLoad(x)
  osgPut2(osgSTW, x.r, x.r, cast[int32](osgStdWriteInt))
  fixlist = osgPc - 1
  dec(RH)
  osgInvalSB()


proc osgWriteLn* =
  ## Standard procedure to output a line feed [CC80].
  ## 
  ## We simplify management of standard functions and procedures, as Nim0
  ## is run in the RISC emulator.
  osgPut2(osgSTW, RH, RH, cast[int32](osgStdWriteLine))
  fixlist = osgPc - 1
  osgInvalSB()


proc osgSwitch*(x: var osgItem) =
  ## Standard function to read the value from a switch [CC80]. Returns its value
  ## as an integer in a register.
  ## 
  ## **NOT IMPLEMENTED IN NIM0**
  osgPut1(osgMOV, RH, 0, -60)
  osgPut2(osgLDW, RH, RH, 0)
  x.mode = osgReg
  x.typ = osgIntType
  x.r = RH
  inc(RH)


proc osgLED*(x: var osgItem) =
  ## Standard procedure to activate a LED [CC80].
  ## 
  ## **NOT IMPLEMENTED IN NIM0**
  osgLoad(x)
  osgPut1(osgMOV, RH, 0, -60)
  osgPut2(osgSTW, x.r, RH, 0)
  dec(RH)


proc osgOpen* =
  ## Set the environment for the compilation of a new Nim0 file.
  osgPc = 0     # Program counter in `osgCode`
  RH = 0        # Bottom of the registers stack
  fixlist = 0   # List of fix-up addresses for BL instructions
  fixorgD = 0   # List of fix-up addresses for LDR instructions


proc osgHeader*(size: int32) =
  ## Generate the code for a module prologue
  entry = osgPc * osgWordSize
  osgPut1(osgSUB, SP, SP, osgWordSize)
  osgPut2(osgSTW, LNK, SP, 0)
  osgInvalSB()


proc osgMakeFileName(FName: var string; name, ext: string) =
  ## Create a file name from base name `name` and extension `ext`.
  FName = name & ext


proc osgWriteInt32(f: File; i: int32) =
  ## Write a binary int to file.
  if writeBuffer(f, unsafeAddr i, osgWordSize) != osgWordSize:
    ossMark("Error writing int32 " & $i & " to file")


proc osgWriteString(f: File; s: string) =
  ## Write a 0-terminated string to file. Save the length of the string
  ## first as `int32` before saving the characters.
  let l = len(s)
  if l >= high(int32):
    ossMark("String too large to be written to file")
  osgWriteInt32(f, int32(l))
  if writeChars(f, s, 0, l) != l:
    ossMark("Error writing string \"" & s & "\" to file")


proc osgClose*(modid: string; key, datasize: int32; topScope: osgObject) =
  ## Write compilation result `osgCode` to file. The file format tries to remain
  ## compatible with Wirth's Oberon project object file format, even if we
  ## don't use some features and we could have simplified it [PO81], but
  ## the binary format is different. For instance, `string`s are stored with
  ## their length while it's not probably the case in Oberon.
  ##
  ## ```
  ## CodeFile = name key version size imports typedesc varsize strings code commands entries ptrrefs fixP fixD fixT body "O".
  ## imports = {modname key} 0X.
  ## typedesc = nof {byte}.
  ## strings = nof {char}.
  ## code = nof {word}.
  ## commands = {comname offset} 0X.
  ## entries = nof {word}.
  ## ptrrefs = {word} 0.
  ## ```
  var
    i: int
    nofent: int32     # Number of file entries?
    nofimp: int32     # Number of files imported?
    comsize: int32    # Memory size for commands
    size: int32       # Memory size to load module?
    nocmd: int32      # Number of commands. Does not exist in Oberon object
                      # file format but makes a simpler RISC emulator.
    obj: osgObject
    name: string
    F: File

  # Module epilogue
  osgPut2(osgLDW, LNK, SP, 0)
  osgPut1(osgADD, SP, SP, osgWordSize)
  osgPut3(BR, AL, LNK)
  
  # Go through the list of top level symbols and check those who are commands.
  obj = topScope.next
  comsize = osgWordSize
  nofent = 1
  nofimp = 1
  nocmd = 0
  while obj != nil:
    if obj.comd:
      i = len(obj.name)     # Count entries and commands
      i = (i + osgWordSize) div osgWordSize * osgWordSize # Align to 4 bytes
      inc(comsize, i + osgWordSize)
      inc(nofent)
      inc(nofimp)
      inc(nocmd)
    obj = obj.next

  size = datasize + comsize + (osgPc + nofimp + nofent + 1) * osgWordSize

  osgMakeFileName(name, modid, ".rsc")
  if not open(F, name, fmWrite):
    ossMark("Can't write to RSC file " & name)

  # RISC object file header
  osgWriteString(F, modid)       # Module name
  osgWriteInt32(F, key)          # Dependency check
  write(F, '\x01')            # version
  osgWriteInt32(F, size)         # Total size of memory in bytes
  osgWriteString(F, "IO")        # Imports name (here module IO in Oberon-0)
  osgWriteInt32(F, 0x3A83_72E2'i32) # Import key (probably value related to module IO)
  write(F, '\x00')            # No more imports
  osgWriteInt32(F, 0'i32)        # No type descriptors
  osgWriteInt32(F, datasize)     # Size of data
  osgWriteInt32(F, 0'i32)        # No strings
  osgWriteInt32(F, osgPc)        # Size of program code
  # Program
  if writeBuffer(F, addr osgCode[0], osgPc * osgWordSize) != osgPc * osgWordSize:
    ossMark("Can't write resulting code to file")

  # Commands (external proc that can be called by the RISC emulator)
  osgWriteInt32(F, nocmd)        # The number of commands entry points
  obj = topScope.next
  while obj != nil:
    if obj.comd:
      osgWriteString(F, obj.name)
      osgWriteInt32(F, obj.val)  # Address of command in bytes
    obj = obj.next
  write(F, '\x00')
  
  # Entries
  osgWriteInt32(F, nofent)       # Number of entries
  osgWriteInt32(F, entry)        # The module entry point
  obj = topScope.next
  while obj != nil:           # Entries procedures
    if obj.comd:
      osgWriteInt32(F, obj.val)  # Address of entry in bytes
    obj = obj.next
  
  osgWriteInt32(F, -1'i32)       # No pointer variables
  osgWriteInt32(F, fixlist)      # Head of fixup list for BL instructions
  osgWriteInt32(F, fixorgD)      # Head of fixup list for LDR/STR/ADD instructions
  osgWriteInt32(F, 0'i32)        # No fixup list for type descriptors
  osgWriteInt32(F, entry)        # ? Not documented
  write(F, 'O')               # End of RISC object file
  close(F)


