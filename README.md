C-style language compiler suite
==================================
This compiler is an modular compiler for C-style languages. It is modularized into the following sections
- **Tokenizer.** Performs correct token syntax checking.
- **Parse tree generator.** Performs complete syntax checking.
- **Semantic checker.** Performs semantic checking on the parse tree, including types, scope, declarations.
- **Code generator.** Performs transformation of the parse tree into assembly.
- **Assembler.** Performs transformation of the assembly into machine code.

The ultimate goal for this compiler is to extensible compiler solution with a complete test suite. The ambition is to create a full compiler and then a simple MIPS emulator, and a suite of tools in order to thoroughly test the validity of the compiler. Creating the compiler in a modular way will provide an easy entrance to implementing other features or compile targets, while retaining the same validity for shared code.

Usage
======

MIPS Assembly Scanner
-------
Tokenizes the MIPS assembly into easy to consume lexical tokens.
```
racket src/assembling/mips/scanner-mips.rkt tests/functional/mips/tokenizer/basic-word.in
```
MIPS Assembler
-------
Assembles MIPS into machine code.
```
racket src/assembling/mips/assember-mips.rkt tests/functional/mips/tokenizer/basic-word.in
```
For readable binary output use the _pretty output_ argument option, `-p`. For debugging information, use the _verbose_ argument options, `-v`.



License
-------
Copyright Joey Pereira (xLegoz) and all contributors.

This software is licensed under the [GNU Affero General Public License](http://www.gnu.org/licenses/agpl-3.0.html).

This means everyone is free to use, modify, and distribute the files, as long as these modifications are also licensed the same way.
Most importantly, the Affero variant of the GPL requires you to publish your modifications in source form.
