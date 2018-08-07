# Cool Compiler

This repository contains my C++ implementation of a COOL compiler.

COOL stands for Classroom Object Oriented Language and is a simple programming language used for teaching compilers in Stanford's [CS143 open course](https://lagunita.stanford.edu/courses/Engineering/Compilers/Fall2014/about). The course project consists of 4 programming assignments (one for each compiler phase) that assembled together constitute a complete COOL compiler.

Among other things COOL is:

* Object oriented supporting inheritance and dynamic dispatching.
* Statically typed, strongly typed and type safe.
* Garbage collected; memory is managed automatically.

## Assignment 1 - Lexical Analysis

Built a lexical analyzer using flex.

## Assignment 2 - Parsing

Generated a LALR parser using bison that builds an abstract syntax tree (AST) for each program.

## Assignment 3 - Semantic Analysis & Type Checking

Wrote the semantic analyzer which also does type checking.
The analyzer reports any semantic errors and annotates the AST with type information.

## Assignment 4 - Code Generation

Built a stack machine code generator for the 32-bit MIPS architecture.
