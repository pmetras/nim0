## RISC emulator for Oberon-0 or Nim-0 object code execution

import OSG
import strutils, hashes, strformat

const
  riscAutoVar = 10              ## This is a hack I created to reserve room for 10
                                ## *automatic* global variables. When using `for i in start .. end`
                                ## for loop, `i` is a variable automatic variable that
                                ## is defined in a new block and whose type is derived from
                                ## `start` type. This variable is allocated in the stack.
                                ## But when the for loop is global, there is no place
                                ## reserved for this declaration. The `riscAutoVar` keeps
                                ## some room for these variables.


type
  riscModule = object
    memSize: int32              ## The memory size required to load the module
    dataSize: int32             ## Memory size for global variables
    codeSize: int32             ## Memory size for code
    commands: seq[string]       ## The module commands that can be called by the user.
    commandAddr: seq[int32]     ## The address in code of the commands entry points
    mem: seq[uint32]            ## The whole RISC module loaded in memory.
    codeAddr: int32             ## Address of code entry point


var
  IR: uint32                    ## Instruction register
  PC: int32                     ## Program counter
  Z: bool                       ## Zero condition flag
  N: bool                       ## Negative condition flag
  R: array[16, int32]           ## Registers
  H: int32                      ## Auxilliary register for division
  clock: int                    ## Clock tick for debug

  riscCmd: string               ## The command to be executed
  riscCmdParams: seq[string]    ## The parameters to the command, that are read by standard functions
  riscCmdIndex = 0              ## Index of input in command line parameters


template until*(cond, body: untyped): untyped =
  ## Reproduce `repeat ... until condition` that exists in Oberon.
  while true:
    body
    if cond:
      break


# Standard functions and procedures

proc riscEndOfInput: int32 =
  ## Check if input index has reached end of input (i.e. there are no
  ## more input to read from command parameters).
  result = if riscCmdIndex >= len(riscCmdParams): 1 else: 0


proc riscGetInt: int32 =
  ## Get next input from command line as an integer
  ## Aborts with error message if the user tries to read after end of input.
  if riscCmdIndex >= len(riscCmdParams):
    writeLine(stderr, "Trying to read after end of input parameters")
    quit(QuitFailure)

  let param = riscCmdParams[riscCmdIndex]
  try:
    let r = parseInt(param)
    if r < low(int32) or r > high(int32):
      writeLine(stderr, param & " value overflow or underflow; must be in range [" & $low(int32) & " .. " & $high(int32) & "]")
      quit(QuitFailure)
    inc(riscCmdIndex)
    result = int32(r)
  except:
    writeLine(stderr, "Calling riscGetInt on non-integer value " & param)
    quit(QuitFailure)


proc riscGetSwitch: int32 =
  ## Read input value from a switch. NOT IMPLEMENTED
  writeLine(stderr, "Switch function is not implemented")
  quit(QuitFailure)


proc riscWriteLED(x: int32) =
  ## Write a value to a LED: NOT IMPLEMENTED
  writeLine(stderr, "LED procedure is not implemented")
  quit(QuitFailure)


proc riscWriteRegisters(W: File) =
  ## Write the content of CPU registers. Used in verbose execution mode.
  for i in 0 ..< len(R):
    osgWriteReg(uint32(i), W)
    if i < 13:
      write(W, "=", $R[i], ", ")
    else:
      write(W, "=", $(cast[uint32](R[i])), "'u32, ")
  write(W, "N=", $N, ", Z=", $Z)


proc riscWriteMemory(W: File; mem: seq[uint32]; adr: int32) =
  ## Write word content of module memory arround `adr` address location.
  ## Used for debug in verbose execution mode.
  for i in adr - 1 .. adr + 1:
    if i >= 0 and i < osgMemSize div osgWordSize:
      writeLine(W, "\t\t\t\t\t\t\tMem[", $i, " | ", $(i * osgWordSize), "]=", $mem[i], " -> ", toHex(mem[i]), (if i == adr: " <" else: ""))


proc riscExecute(module: var riscModule; pc: int32; sp: int32; S: File; W: File; verbose: bool) =
  ## Execute some code from the module memory, starting at address `pc` and
  ## using `sp` as stack pointer to runtime stack.
  ## 
  ## We support only WORD (32 bits) instructions or memory addresses. Memory
  ## can't be byte-addressed in this emulator. The code is a bit trick because
  ## memory location is displayed indexed by 32-bits words when dealing with
  ## instructions but 8-bits bytes when dealing with addesses...
  ## 
  ## In this part of the code, `PC` is not a 32 bits addresses but the index
  ## in `module.mem` array. If you want to convert it to an address, you must
  ## multiply it by `osgWordSize`.
  ## 
  ## Signed/unsigned dance: it is easier to consider module memory containing
  ## code as containing unsigned values (`uint32`) while registers contains
  ## signed (`int32`) values corresponding to the integer variables of Nim0.
  var
    a, b, op, im: uint32  # instruction fields
    adr, A, B, C: int32
    M = module.mem        # Memory = Globals + Program code + Runtime stack

  PC = pc div osgWordSize    # Program Counter
  R[13] = 0                           # Static Base [CC51] because we place global data before code
  R[14] = sp                          # Runtime Stack Pointer
  R[15] = 0                           # Return address to end execution

  until PC == 0: # Interpretation cycle
    # Debug: show source
    if verbose:
      write(W, "\t\t\t\t\tPC=" & $PC & ", ")
      riscWriteRegisters(W)
      writeLine(W, "")
      write(W, &"{clock:>06}\t")
      osgWriteDebugAsm(PC, M, W)
      writeLine(W, "")

    IR = M[PC]  # Load the instruction register
    inc(PC)     # Increment program counter
    inc(clock)  # Tick clock

    a = (IR and 0x0F00_0000'u32) shr 24
    b = (IR and 0x00F0_0000'u32) shr 20
    op = (IR and 0x000F_0000'u32) shr 16
    im = (IR and 0x0000_FFFF'u32)

    # Register instruction
    if (IR and 0x8000_0000'u32) == 0: # Bit 31 is set
      B = cast[int32](R[b])
      if (IR and 0x4000_0000'u32) == 0: #[ ~q ]#
        C = cast[int32](R[IR and 0x0000_000F'u32])
      elif (IR and 0x1000_0000'u32) == 0: #[ q&~v ]#
        C = cast[int32](im)
      else: #[ q&v ]#
        C = cast[int32](im or 0xFFFF_0000'u32)
      
      case op
        of osgMOV:
          if (IR and 0x2000_0000'u32) == 0: # u
            A = C
          else:
            A = cast[int32](H) # Get modulo from division in H register
        of osgLSL:
          A = B shl C
        of osgASR:
          A = ashr(B, C)
        of osgROR:
          # Rotate bits on right C times
          let x = C and 31
          A = (B shr x) or (B shl ((-x) and 31))
        of osgAND:
          A = B and C
        of osgANN:
          A = B and not C
        of osgIOR:
          A = B or C
        of osgXOR:
          A = B xor C
        of osgADD:
          A = B + C
        of osgSUB:
          A = B - C
        of osgMUL:
          A = B * C
        of osgDIV:
          A = B div C
          H = B mod C
        else:
          writeLine(stderr, "Invalid instruction:", op)

      # Set registers and flags
      R[a] = A
      N = A < 0
      Z = A == 0

    # Load/store instruction
    elif (IR and 0xC000_0000'u32) == 0x8000_0000'u32:
      let off = IR and 0x000F_FFFF'u32
      case off
      of osgStdReadInt:   # Read next integer from command line
        R[a] = riscGetInt()
      of osgStdEot:       # End of input
        R[a] = riscEndOfInput()
        Z = A == 0
      of osgStdWriteInt:  # Write integer
        write(W, cast[int32](R[a]))    # Signed values from register are converted back to unsigned code memory
      of osgStdWriteChar: # Write ASCII character
        let c = cast[uint32](R[a]) and 0x0000_00FF'u32
        write(W, chr(c))
      of osgStdWriteBool: # Write boolean value
        write(W, cast[int32](R[a]) != 0)
      of osgStdWriteLine: # Write end of line
        writeLine(W, "")
      of osgStdOpenInput:
        writeLine(stderr, "openInput (NOP)")
      of osgStdLED:
        writeLine(stderr, "LED (NOP")
      of osgStdSwitch:
        writeLine(stderr, "readSwitch (NOP")
      else:
        adr = (R[b] + cast[int32](IR and 0x000F_FFFF'u32)) div osgWordSize
        if verbose:
          riscWriteMemory(W, M, adr)

        if (IR and 0x2000_0000'u32) == 0: # Load
          R[a] = cast[int32](M[adr])  # Unsigned values from code memory are converted to signed register values
          N = A < 0
          Z = A == 0
        else: # Store
          M[adr] = cast[uint32](R[a])
          if verbose:
            writeLine(W, "\t\t\t\t\t\tAfter------------")
            riscWriteMemory(W, M, adr)

    # Branch instructions
    else:
      if (a == 0) and N or (a == 1) and Z or (a == 5) and N or (a == 6) and (N or Z) or (a == 7) or
         (a == 8) and not N or (a == 9) and not Z or (a == 13) and not N or (a == 14) and not (N or Z):
        if (IR and 0x1000_0000'u32) != 0: # v == 1
          R[15] = PC * osgWordSize  # Load link (return value)
        if (IR and 0x2000_0000'u32) != 0: # u == 1)
          var off = cast[int32](IR and 0x00FF_FFFF'u32)
          if off >= 0x0080_0000'i32:
            off = off - 0x0100_0000'i32
          off = off
          PC = PC + off # Don't add 1 as PC has already been incremented
        else:
          PC = R[IR and 0x0000_000F'u32] div osgWordSize


proc riscReadInt32(f: File): int32 =
  ## Read a binary `int32` from file `f` and return its value.
  if readBuffer(f, addr result, osgWordSize) != osgWordSize:
    raise newException(IOError, "Can't read integer")


proc riscReadString(f: File): string =
  ## Read a binary `string` from file `f` and return its value.
  var length: int32 = 0
  if readBuffer(f, addr length, sizeof(length)) != sizeof(length):
    raise newException(IOError, "Can't read string length")
  result = newString(length)
  if readChars(f, result, 0, length) != length:
    raise newException(IOError, "Can't read string")


proc riscLoadCode(f: File; module: var riscModule; W: File; verbose: bool) =
  ## Load the RISC object file in memory and returns the loaded memory.
  ## 
  ## The memory is organized:
  ## - Global variables
  ## - Code
  ## - Stack
  if module.dataSize + module.codeSize + riscAutoVar * osgWordSize + 100 > osgMemSize:
    writeLine(stderr, "WARNING: Runtime stack size less than 100 bytes. Change `osgMemSize` constant value!")
  module.mem = newSeqUninitialized[uint32](osgMemSize div osgWordSize)

  # Initialize global data segment to 0
  let dataSize = module.dataSize div osgWordSize
  for i in 0 ..< dataSize:
    module.mem[i] = 0
  if verbose:
    writeLine(W, "Global variables in memory adresses Mem[0 .. ", $(dataSize - 1), "]")

  # Now load code.
  let bottomCode = dataSize
  let topCode = module.codeSize div osgWordSize + bottomCode
  try:
    var i = bottomCode
    while not endOfFile(f) and i < topCode:
      module.mem[i] = cast[uint32](riscReadint32(f))
      inc(i)
    if verbose:
      writeLine(W, "Module code in memory adresses      Mem[", $bottomCode, " .. ", $(topCode - 1), "]")
  except:
    writeLine(stderr, "RISC file read error: end of file reached before end of code")
    quit(QuitFailure)

  # For simpler debug, initialize stack area with pattern that can't be
  # converted to `int32`.
  for i in topCode ..< len(module.mem):
    module.mem[i] = 0xAAAA_AAAA'u32
  if verbose:
    writeLine(W, "Runtime stack in memory adresses    Mem[", $topCode, " .. ", $(len(module.mem) - 1), "]")
    writeLine(W, "")

  # For code termination, the global Link address is stored at top of stack
  # and must be 0. This hack is to accomodate Oberon-0 module prologue.
  module.mem[len(module.mem) - 1] = 0


proc riscFixupBL(module: var riscModule; fixlist: int32) =
  ## Fix-up BL instruction now that we know where the code is loaded in memory
  ## [CC96].
  ## 
  ## **NOT USED IN NIM0**
  # Offset of `module.dataSize` to be at start of code.
  var adr = module.dataSize + fixlist * int32(osgWordSize)
  while adr != module.dataSize:
    let inst = module.mem[adr div osgWordSize]
    assert (inst and 0xD000_0000'u32) != 0, "Not on BL instruction!"
    let dest = cast[int32](inst and 0x00FF_FFFF'u32) # + module.codeAddr
    let offset = (dest - adr - 1) div int32(osgWordSize)
    module.mem[adr div osgWordSize] = (module.mem[adr div osgWordSize] and 0xFF00_0000'u32) or (cast[uint32](offset) and 0x00FF_FFFF'u32)
    adr = adr - offset * int32(osgWordSize)


proc riscFixupLDR(module: var riscModule; fixorgD: int32) =
  ## Fix-up LDR instructions now that we know where the code is loaded in memory
  ## [CC96] and [CC74].
  ## 
  ## **NOT USED IN NIM0**
  # Offset of `module.dataSize` to be at start of code.
  var adr = module.dataSize + fixorgD * int32(osgWordSize)
  while adr != module.dataSize:
    let inst = module.mem[adr div osgWordSize]
    assert (inst and 0x8000_0000'u32) != 0, "Not on LDR/STR/ADD instruction!"
    let dest = cast[int32](inst and 0x000F_FFFF'u32) # + module.codeAddr
    let offset = (dest - adr - 1) div int32(osgWordSize)
    module.mem[adr div osgWordSize] = (module.mem[adr div osgWordSize] and 0xFFF0_0000'u32) or (cast[uint32](offset) and 0x000F_FFFF'u32)
    adr = adr - offset * int32(osgWordSize)


proc riscLoadModule(target: string; file: File; W: File; verbose: bool): riscModule =
  ## Start loading data from stream and initialise `riscModule` structure
  ## to load code and link for execution. Display the final RISC assembler code
  ## if `verbose` is set.
  try:
    let t = if target.endsWith(".rsc"): substr(target, 0, len(target) - 5) else: target
    let modid = riscReadString(file)
    if t != modid:
      writeLine(stderr, "Warning: " & target & " has been compiled from " & modid & " module")

    let key = riscReadInt32(file)
    if key != cast[int32](hash(modid)): # Dependencies checks with key
      raise newException(ValueError, "Found key " & $key & " != " & $cast[int32](hash(t)) & " expected")
    if readChar(file) != '\x01': # 0x1
      raise newException(ValueError, "Version != 1")
    result.memSize = riscReadInt32(file)
    # IO information
    if riscReadString(file) != "IO":
      raise newException(ValueError, "IO")
    if riscReadInt32(file) != 0x3A83_72E2'i32:
      raise newException(ValueError, "0x3A83_72E2 magic value")
    if readChar(file) != '\x00':
      raise newException(ValueError, "Imports")
    if riscReadInt32(file) != 0:
      raise newException(ValueError, "Type descriptors")
    result.dataSize = riscReadInt32(file)
    if riscReadInt32(file) != 0:
      raise newException(ValueError, "Strings")
    result.codeSize = riscReadInt32(file) * int32(osgWordSize)

    # Caution: Hack!
    # We make the code start at address 1 instead of 0 because `PC == 0` is the condition
    # to end emulation loop. This offset is necessary only when there are no global variables
    # defined in the Nim0 program. So we create a dummy global if necessary.
    if result.dataSize == 0:
      result.dataSize = osgWordSize

    # Load code and allocate module memory at the same time
    riscLoadCode(file, result, W, verbose)

    # Commands
    var nocmd = riscReadInt32(file)
    for i in 1 .. nocmd:
      var riscCmd = riscReadString(file)
      result.commands.add riscCmd
      result.commandAddr.add(riscReadInt32(file) + result.dataSize)
    if readChar(file) != '\x00':
      raise newException(ValueError, "Commands")

    # Entries
    discard riscReadInt32(file)     # Number of entries?
    result.codeAddr = riscReadInt32(file) + result.dataSize # Address of module entry
    for i in 0 ..< nocmd:
      discard (riscReadInt32(file) + result.dataSize)   # Entry addresses
    
    # Pointers
    if riscReadInt32(file) != -1:
      raise newException(ValueError, "Pointers are not supported")
    let fixlist = riscReadInt32(file)
    let fixorgD = riscReadInt32(file)
    if riscReadInt32(file) != 0:
      raise newException(ValueError, "Fixup type descriptors")
    if riscReadInt32(file) != result.codeAddr - result.dataSize:
      raise newException(ValueError, "Entry")
    if readChar(file) != 'O':
      raise newException(ValueError, "End of object file marker")

    # Fix-up memory addresses for BL and LDR/STR/ADD instructions
    # There is no need for these linker operations in Nim0 as our memory
    # model is simplified.
    #riscFixupBL(result, fixlist)
    #riscFixupLDR(result, fixorgD)

    # Show source if debug
    if verbose:
      writeLine(W, "Resulting RISC assembler")
      for i in int32(result.dataSize div osgWordSize) ..< int32((result.dataSize + result.codeSize) div osgWordSize):
        osgWriteDebugAsm(i, result.mem, W)
        writeLine(W, "")

  except:
    writeLine(stderr, "RISC file header is not correct: " & getCurrentExceptionMsg())
    quit(QuitFailure)


proc riscRun*(target: string; args: seq[string]; verbose: bool): int =
  ## Load `source` RISC object file and execute command given by `args`.
  var m = target
  if not m.endsWith(".rsc"):
    m = m & ".rsc"
  var file: File
  if open(file, m, fmRead):
    var module: riscModule
    var topOfStack: int32 = 0
    try:
      # Read RISC object file
      module = riscLoadModule(target, file, stdout, verbose)
      topOfStack = int32((len(module.mem) - riscAutoVar) * osgWordSize)

      if verbose:
        echo ""
        echo "Starting execution..."
      clock = 1

      # First initialize module data by executing module code. We used 32 bits addresses
      # in the emulator contrarily to Oberon-0 and that's the reason why we divide by
      # osgWordSize.
      riscExecute(module, module.codeAddr, topOfStack, stdin, stdout, verbose)
      if verbose:
        echo "riscModule initialized and completed"

      # If the user did not supplied a command name, we can stop
      if len(args) < 1:
        quit(QuitSuccess)

      # If the user supplied a command, then execute it
      riscCmd = args[0]
      riscCmdParams = if len(args) > 1: args[1 .. ^1] else: @[]
      var cmdAddr: int32 = -1

      if verbose:
        echo "Launching ", riscCmd, " from ", target, " with parameters ", riscCmdParams

      for i in 0 ..< len(module.commands):
        if riscCmd == module.commands[i]:
          cmdAddr = module.commandAddr[i]
          break
      if cmdAddr == -1:
        writeLine(stderr, "Can't find command " & riscCmd & " in " & target & " entries. Check that the proc is exported")
        quit(QuitFailure)

      riscExecute(module, cmdAddr, topOfStack, stdin, stdout, verbose)
      if verbose:
        echo "Execution completed"
      result = QuitSuccess

    except:
      # In case of runtime error, dump content of registers and current instruction.
      writeLine(stderr, "Runtime error: " & getCurrentExceptionMsg())
      write(stderr, "PC=" & $(PC - 1) & ", ")
      riscWriteRegisters(stderr)
      writeLine(stderr, "")
      osgWriteDebugAsm((PC - 1), module.mem, stderr)
      writeLine(stderr, "")
      result = QuitFailure
  else:
    writeLine(stderr, "Can't read RISC object file ", m)
    result = QuitFailure