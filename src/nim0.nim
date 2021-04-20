## Nim0 compiler driver
## 
## ```
## Usage: nim0 action [options]* source [args]*
## 
## action    comp  Compile Nim0 source
##           run   Excute source object file
## 
## options   -v    Show generated assembler after compilation or the current
##                 instruction being executed by the RISC emulator.
## 
## source          When compiling, Nim0 source code. Assume .nim0 extension.
##                 When execution, RISC object file. Assume .rsc extension
## 
## args            Command to be executed and parameters.
## ```


import OSP, RISC
import os

var
  action: string          # The action to be executed: compile or run
  target: string          # The target of the action
  verbose = false         # Show resulting RISC assembler
  args: seq[string]       # Commands to be executed


proc usage =
  ## Print Nim0 usage on stderr
  writeLine(stderr, """nim0 compiler and runtime.
Usage: nim0 action [options]* source [args]*

action    comp  Compile Nim0 source
          run   Excute source object file

options   -v    Show generated assembler after compilation or the current
                instruction being executed by the RISC emulator.

source          When compiling, Nim0 source code. Assume .nim0 extension.
                When execution, RISC object file. Assume .rsc extension

args            Command to be executed and parameters.
""")


proc parseParams =
  ## Process command line parameters and set the module
  ## global variables accordingly.
  var params = commandLineParams()

  # At least 2 parameters are required: action + filename
  if len(params) < 2:
    usage()
    quit(QuitFailure)

  action = params[0]
  for i in 1 ..< len(params):
    if params[i] == "-v":
      verbose = true
    elif target == "":
      if params[i][0] == '-':
        writeLine(stderr, "Unknown command line option " & params[i])
        writeLine(stderr, "")
        usage()
        quit(QuitFailure)
      target = params[i]
    else:
      args.add params[i]


proc driver =
  ## Select between actions
  parseParams()

  if action == "comp":
    let status = ospCompile(target, verbose)
    quit(status)
  elif action == "run":
    let status = riscRun(target, args, verbose)
    quit(status)
  if action != "comp" and action != "run":
    usage()
    quit(QuitFailure)


# Let's' go!
driver()