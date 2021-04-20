#
# NimScript build file for Nim0
#

const
  url = " --docSeeSrcUrl:http://gitlab.com/pmetras/nim0/"


# Switches
when not defined(release):
  switch("verbosity", "1")
  switch("hints", "on")
  switch("debugger", "native")

when defined(release):
  switch("d", "release")
  switch("verbosity", "0")
  switch("hints", "off")


#
# Tasks
#

task build, "Build Nim0 compiler environment":
  exec "nim c -o:nim0 src/nim0.nim"


task buildExamples, "Build all Nim0 examples":
  exec "./nim0 comp examples/Hello"
  exec "./nim0 comp examples/Factorial"
  exec "./nim0 comp examples/MagicSquares"
  exec "./nim0 comp examples/Maths"
  exec "./nim0 comp examples/PrimeNumbers"
  exec "./nim0 comp examples/Sudoku"


task buildDoc, "Build Nim0 documentation":
  exec "nim doc --outdir:doc" & url & " src/nim0.nim"
  exec "nim doc --outdir:doc" & url & " src/OSS.nim"
  exec "nim doc --outdir:doc" & url & " src/OSP.nim"
  exec "nim doc --outdir:doc" & url & " src/OSG.nim"
  exec "nim doc --outdir:doc" & url & " src/RISC.nim"

