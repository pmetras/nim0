---
title: Nim0, a compiler for a subset of Nim language to a RISC CPU with an emulator
author: Pierre Métras <pierre@alterna.tv>
date: 2021-04-21
keywords: compiler, construction, Niklaus Wirth, nim, toy, language, RISC, emulator, Oberon-0
lang: en
gitroot: https://gitlab.com/pmetras/nim0/-/blob/master/
webroot: https://pmetras.gitlab.io/nim0/
---

# Nim0: a minimal Nim compiler

So you want to hack the [Nim compiler](https://github.com/nim-lang/Nim/) but you are afraid by the complexity of the project? Look at Nim0 first to understand compiler basis. Try to add a new feature on your own, and then jump on the real project.

Nim0 is a toy compiler for a limited subset of [Nim language](https://nim-lang.org/), all in 5 heavily documented source files (less than 4k LOC) so that you can understand them. It is a port of [Niklaus Wirth's Oberon-0 compiler](https://www.projectoberon.org/). You can read Wirth's book [Compiler Construction](misc/CompilerConstruction.pdf) provided in the `misc` directory and follow in the code.

The compiler can translate Nim0 source code to RISC 32 bits machine instructions (yes, that's a 32 platform and it makes the Nim code a bit more complex). A RISC emulator lets you run the resulting code, and you can even display and follow the content of the registers and RISC assembler instructions.

Nim0 language is a tiny subset of Nim, but the goal is that Nim0 code is valid Nim code. Just change the `.nim0` extension to `.nim` to run it with a serious compiler (You'll probably need to write a few templates to adapt Nim0 I/O procedures to Nim)!

## Nim0 features

Compared with Nim, Nim0 implements only minimal features to understand compiler construction:

- Only three basic types: `bool`, `char` and `int`. No `string` or `seq`, no advanced data structures. But `array` and *simple* `object` are supported.
- The list of missing Nim feature is too long to be complete, because Nim is a complex language while Nim0 is a toy one to understand compilation. No imports. No libraries. The system procedures are limited to a handfull of functions. No generics. No overloading but for a few I/O procedures and functions. No templates. No macros. No closures. Limited `for` loops. No `return` instruction! No exceptions. No access to intermediate variables in enclosed procedures. No all these advanced features that you use with Nim.
- Source code is limited to 1000 lines. Generated code is limited to 8 KB.
- A one-pass compiler that generates RISC instructions.
- A RISC emulator to execute generated code.
- More strict on syntax and a few other differences like identifiers are case-sensitive...
- We have also sometimes *optimized* the Nim code compared with Oberon's one used in the [Compiler Construction](misc/CompilerConstruction.pdf) book when Nim syntax was easier to read but we tried to limit ourself to respect Wirth's style in the book.

Can you write real programs with such limited features? Have a look at the [examples](examples/)...

## Usage

```out
nim0 compiler and runtime.
Usage: nim0 action [options]* source [args]*

action    comp  Compile Nim0 source
          run   Excute source object file

options   -s    Show generated assembler after compilation.

source          When compiling, Nim0 source code. Assume .nim0 extension.
                When execution, RISC object file. Assume .rsc extension

args            Command to be executed and parameters.
```

## Pre-requisites

Nim0 has been developed and tested on Linux with Nim 1.5. I don't know if it works on Windows or other environments. It'll probably require small adjustments. If you have a Nim compiler on your Linux machine, you can try it right now!

## Example

Let's try it by yourself. First compile is to create the Nim0 compiler (look at [config.nims](config.nims) for the project's other tasks).

```sh
nim0$ nim build
```

It creates the `nim0` executable in the current directory.

Now, write your first Nim0 program and name it `examples/Alpha0.nim0` (You're lucky, I typed it for you; look into the [examples](examples/) folder).

```nim
## My first Nim0 program: print integer squares in range 0 ... 10.

var
  i: int
  r: int

proc sqr(x: int; sqrX: var int) =
  # Square the value of `x` and return it in `sqrX`.
  sqrX = x * x

for i in 0 .. 10:
  sqr(i, r)
  write(i)
  write('*')
  write(i)
  write('=')
  write(r)
  writeLine()
```

You can compile your first Nim0 program now:

```out
nim0$ ./nim0 comp examples/Alpha0
Nim0 Compiler v1.0
  compiling examples/Alpha0.nim0
code generated examples/Alpha0, PC=40, dc=8, key=-1973735804
```

If you want to see the generated RISC assembler code, add the `-v` option.

```out
nim0$ ./nim0 comp -v examples/Alpha0
Nim0 Compiler v1.0
  compiling examples/Alpha0.nim0
code generated examples/Alpha0, PC=40, dc=8, key=-1973735804
4EE9000CAFE00000
                                        # 001 ## My first Nim0 program: print integer squares in range 0 ... 10.
                                        # 002 
                                        # 003 var
                                        # 004   i: int
                                        # 005   r: int
                                        # 006 
                                        # 007 proc sqr(x: int; sqrX: var int) =
0000 4EE9000C        SUB SP, SP, 12
                                        # 008   # Square the value of `x` and return it in `sqrX`.
                                        # 009   sqrX = x * x
0001 AFE00000        STW LNK, SP, 0
0002 A0E00004        STW R0, SP, 4
0003 A1E00008        STW R1, SP, 8
                                        # 010 
0004 80E00004        LDW R0, SP, 4
0005 81E00004        LDW R1, SP, 4
0006 000A0001        MUL R0, R0, R1
0007 81E00008        LDW R1, SP, 8
0008 A0100000        STW R0, R1, 0
0009 8FE00000        LDW LNK, SP, 0
0010 4EE8000C        ADD SP, SP, 12
0011 C700000F        B LNK
0012 4EE90004        SUB SP, SP, 4
0013 AFE00000        STW LNK, SP, 0
0014 40000000        MOV R0, 0
0015 A0D00000        STW R0, SB, 0
                                        # 011 for i in 0 .. 10:
0016 80D00000        LDW R0, SB, 0
0017 4009000A        CMP R0, 10
0018 EE000012        BGT 18
0019 80D00000        LDW R0, SB, 0
                                        # 012   sqr(i, r)
0020 41D80004        ADD R1, SB, 4
0021 F7FFFFEA        BL -22
                                        # 013   write(i)
0022 80D00000        LDW R0, SB, 0
0023 A00FFFFC        writeInt <- R0
                                        # 014   write('*')
0024 4000002A        MOV R0, 42
0025 A00FFFFB        writeChar <- R0
                                        # 015   write(i)
0026 80D00000        LDW R0, SB, 0
0027 A00FFFFC        writeInt <- R0
                                        # 016   write('=')
0028 4000003D        MOV R0, 61
0029 A00FFFFB        writeChar <- R0
                                        # 017   write(r)
0030 80D00004        LDW R0, SB, 4
0031 A00FFFFC        writeInt <- R0
                                        # 018   writeLine()
0032 A00FFFFA        writeLine
0033 80D00000        LDW R0, SB, 0
0034 40080001        ADD R0, R0, 1
0035 A0D00000        STW R0, SB, 0
0036 E7FFFFEB        B -21
0037 8FE00000        LDW LNK, SP, 0
0038 4EE80004        ADD SP, SP, 4
0039 C700000F        B LNK
```

OK. Now let's run the compiled code:

```out
nim0$ ./nim0 run examples/Alpha0
0*0=0
1*1=1
2*2=4
3*3=9
4*4=16
5*5=25
6*6=36
7*7=49
8*8=64
9*9=81
10*10=100
```

You can even trace program execution and see the content of the [RISC registers](misc/RISC-Arch.pdf). Before program execution, Nim0 dumps the RISC assembler after code relocation in memory, showing the areas for global variables, code and execution stack. As it is quite verbose, I dump only the most representative parts in the truncated dump below.

```out
nim0$ ./nim0 run -v examples/Alpha0
Global variables in memory adresses Mem[0 .. 1]
Module code in memory adresses      Mem[2 .. 41]
Runtime stack in memory adresses    Mem[42 .. 2047]

Resulting RISC assembler
0002 4EE9000C        SUB SP, SP, 12
0003 AFE00000        STW LNK, SP, 0
0004 A0E00004        STW R0, SP, 4
0005 A1E00008        STW R1, SP, 8
... truncated ...

Starting execution...
                                        PC=14, R0=0, R1=0, R2=0, R3=0, R4=0, R5=0, R6=0, R7=0, R8=0, R9=0, R10=0, R11=0, R12=0, SB=0'u32, SP=8152'u32, LNK=0'u32, N=false, Z=false
000001        0014 4EE90004        SUB SP, SP, 4
                                        PC=15, R0=0, R1=0, R2=0, R3=0, R4=0, R5=0, R6=0, R7=0, R8=0, R9=0, R10=0, R11=0, R12=0, SB=0'u32, SP=8148'u32, LNK=0'u32, N=false, Z=false
000002        0015 AFE00000        STW LNK, SP, 0
                                                        Mem[2036 | 8144]=2863311530 -> AAAAAAAA
                                                        Mem[2037 | 8148]=2863311530 -> AAAAAAAA <
                                                        Mem[2038 | 8152]=2863311530 -> AAAAAAAA
                                                After------------
                                                        Mem[2036 | 8144]=2863311530 -> AAAAAAAA
                                                        Mem[2037 | 8148]=0 -> 00000000 <
                                                        Mem[2038 | 8152]=2863311530 -> AAAAAAAA
                                        PC=16, R0=0, R1=0, R2=0, R3=0, R4=0, R5=0, R6=0, R7=0, R8=0, R9=0, R10=0, R11=0, R12=0, SB=0'u32, SP=8148'u32, LNK=0'u32, N=false, Z=false
000003        0016 40000000        MOV R0, 0
                                        PC=17, R0=0, R1=0, R2=0, R3=0, R4=0, R5=0, R6=0, R7=0, R8=0, R9=0, R10=0, R11=0, R12=0, SB=0'u32, SP=8148'u32, LNK=0'u32, N=false, Z=true
000004        0017 A0D00000        STW R0, SB, 0
... truncated ...
```

To compile all Nim0 supplied examples, run the Nim task `buildExamples`. Look at the example source to see how to invoke it: some of them use the notion of Oberon command as defined in Wirth's documents, while others are more like Nim scripts.

```sh
nim0$ nim buildExamples
Nim0 Compiler v1.0
  compiling examples/Hello.nim0
code generated examples/Hello, PC=59, dc=0, key=1338925179
Nim0 Compiler v1.0
  compiling examples/Factorial.nim0
code generated examples/Factorial, PC=49, dc=8, key=982993432
Nim0 Compiler v1.0
  compiling examples/MagicSquares.nim0
code generated examples/MagicSquares, PC=116, dc=0, key=-90451726
Nim0 Compiler v1.0
  compiling examples/Maths.nim0
code generated examples/Maths, PC=386, dc=44, key=201681063
Nim0 Compiler v1.0
  compiling examples/PrimeNumbers.nim0
code generated examples/PrimeNumbers, PC=235, dc=0, key=1527623905
Nim0 Compiler v1.0
  compiling examples/Sudoku.nim0
code generated examples/Sudoku, PC=492, dc=328, key=-2053649788
```

## Nim0 EBNF syntax

We use [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) to provide the syntax of the Nim0 language.

As a quick reminder, "|" is used to define alternatives. "{ ... }" describes terms that can be omitted or repeated. "[ ... ]"  is used for optional terms. Terminal strings are presented between quotes.

In Nim0 like in Nim, spaces are part of the grammar. Indentation is represented using the `IND>`, `IND=` and `DED` pseudo terms with a context value (see [Nim documentation](https://nim-lang.org/docs/manual.html#lexical-analysis-indentation) for detailed explanation):

- `IND=` Same level of indentation as the preceding line.
- `IND>` Must be indented relative to preceding line.
- `DED` Pops out the indent level from the indent stack.

```EBNF
ident = letter {letter | digit}.
integer = digit {digit}.
comment = "#" {chars ignored to the end of the line}

selector = {"." ident | "[" expression "]"}.
number = integer.
factor = ident selector | number | "(" expression ")" | "not" factor.
term = factor {("*" | "div" | "mod" | "and") factor}.
simpleExpression = ["+" | "-"] term {("+" | "-" | "or") term}.
expression = simpleExpression [("=" | "!=" | "<" | "<=" | ">" | ">=") simpleExpression].
assignment = ident selector "-" expression.
actualParameters = "(" [expression {"," expression}] ")".
procedureCall = ident [actualParameters].
ifStatement = "if" expression ":" IND> statementSequence {IND= "elif" expression ":" IND> statementSequence} [IND= "else:" IND> statementSequence] DED.
whileStatement = "while" expression ":" IND> statementSequence DED.
untilStatement = "until" expression ":" IND> statementSequence DED.
forStatement = "for" ident "in" expression ".." expression ":" IND> statementSequence DED.
blockStatement = "block" ":" IND> statementSequence DED.
statement = [assignment | procedureCall | ifStatement | whileStatement | untilStatement | forStatement | blockStatement].
statementSequence = statement {IND= statement}.
identList = ident {"," ident}.
arrayType = "array" "[" expression "," type "]".
fieldList = [identList ":" type].
objectType = "object" IND> fieldList {"," fieldList} DED.
type = ident | arrayType | objectType.
fpSection = identList ":" ["var"] type.
formalParameters = "(" [fpSection {";" fpSection}] ")".
procedureHeading = "proc" ident [formalParameters].
procedureBody = [declaration | statementSequence].
procedureDeclaration = procedureHeading "=" IND> procedureBody DED.
declarations = ["const" IND> {IND= ident "=" expression} DED] | ["type" IND> {IND= ident "=" type} DED] | ["var" IND> {IND= identList ":" type} DED] | {procedureDeclaration}.
module = declarations [statementSequence].
```

The `until` control structure does not exist in Nim but is present in Oberon-0 and was kept in the code because it can be useful sometimes (it is used a few times in the Nim0 compiler). It can be easily defined in Nim as:

```nim
template until*(cond, body: untyped): untyped =
  ## Reproduces Oberon-0 `REPEAT body UNTIL cond;`
  while true:
    body
    if cond:
      break
```

## The source files

- [`OSS.nim`](/doc/OSS.html): The compiler scanner whose role is to parse lexems: identifiers, keywords and numbers.
- [`OSP.nim`](/doc/OSP.html): The compiler parser, that defines the syntax of the Nim0 language.
- [`OSG.nim`](/doc/OSG.html): The code generator for the RISC target machine.
- [`RISC.nim`](/doc/RISC.html): The RISC processor emulator, used to execute compiled programs.
- [`nim0.nim`](/doc/nim0.html): The Nim0 compiler driver.

All source files are cross-indexed with Wirth's [Compiler Construction](misc/CompilerConstruction.pdf) book. Reference like `[CC13]` refers to page 13 of this document. Procedure and variable names are based on Wirth's original names, but a module prefix has been appended for public names and procedures. For instance, `ossId` is defined in module `OSS.nim` in Nim0 while the original name is `id`.

## Differences with Wirth's compiler

- Of course, we use Nim instead of Oberon code.
- Better error messages.
- Option to dump generated RISC assembler after compilation, with reference to source code.
- I've tried to keep with Wirth's style of code as much as possible even if a better Nim style would have been possible, particularly in the RISC.nim module. But there are many places where Wirth uses hacks (like reusing the same object field for multiple different usages) or that are not clearly documented (like the standard functions and procedures or module management or are hardware dependant). These places in code could be improved.
- A RISC emulator to execute the generated code. The verbose mode shows the resulting assembler and the memory layout, and then executes code line by line with dump of registers and memory.

## Major differences between Nim0 and Nim

Nim0 is really a **toy** language based on Niklaus Wirth's Oberon-0 with a nim-like syntax. Don't expect too much! But it can be used to write small programs and understand how they are compiled to RISC machine instructions.

- Declarations (`const`, `type` and `var` sections) must be before statements. Particulary, they can't be mixed: you have to declare variables before the first instruction.
- Only 1 instruction per line.
- `func` are not supported, so no `result` variable nor `return`. Remember that you must call `readInt(n)` instead of `n = readInt()`!
- Contrarily to Nim, local variables are not initialized to default value when declared. Don't forget to initialize them! This behaviour could easily been corrected but it allows to find uninitialized variables. It's a poor replacement for Nim compiler warnings about unitialized variables.

## Bugs and TODO

There are a few bugs, of course, as debugging a compiler is not a simple operation and I did not construct a complete test suite. Here are some bugs that you can try to correct...

- When displaying the RISC assembler resulting of a compilation, the original Nim0 source lines are not always preceding the assembler they relate to. There are many situations where the assembler line starts before the Nim0 source line. This is due to the simple algorithm used to match RISC assembler and Nim0 source lines that does not take into account the delayed code generation by the compiler. The algorithm would be more complex instead of 4 calls to `ossSetDebugPc`.
- The last line of the source file must be an empty line, else end of file is not correctly detected by the lexer.
- Contrarily to Nim, Nim0 compiler tries to resynchronize in case of syntax errors. These synchronization steps can drive the compiler into infinite loops. It would be nice to have a more robust synchronization algorithm.

## How to extend it?

Of course, you can add more features to make Nim0 more Nim compatible... Wirth lists exercises at the end of each chapter of [Compiler Construction](misc/CompilerConstruction.pdf), to add new types or control structures, or even standard procedures. You just have the choice.

I've left some parts of Wirth's code that can be used to relocate code if you want to support a non fixed-address memory model and play with code relocation or modules, like a system linker. You can decide also to produce better RISC code: there are many parts where you can generate better assembler, without resorting to a syntax tree (AST) like in Nim.

Also, you can make the language safer. For instance, there are no checks that a variable is initialized before use nor that an index is within bounds of an array. Or you can check for overflows when doing calculations.

I haven't completed the for loop to support automatic variable declaration as I was short for time. You can find a draft in function `ospForStatement2`.

## License

Nim0 is created under the [MIT license](LICENSE.md).
