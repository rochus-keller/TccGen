This project aims to develop a reusable compiler backend for the i386, x86_64, ARM and AArch64 architecture based on the Tiny C compiler (TCC).

The idea is to define and implement an intermediate representation language (IR) on bytecode level which includes type and control flow information, with similar features like the C language, in a compact (binary) format easy to generate and parse. The backend reads this intermediate representation and generates object, executable or shared library object files compatible with the C ABI of the target platform.

#### Planned features

- [x] Analyze, reorganize, untangle, trim, modularize the code (not striving for perfection)
- [x] Frontend/backend separation with a clear, minimal API and a decent modularization of the code
- [x] Minimal, refactored C frontend to help understand and document the use of the backend API
- [ ] Implement a frontend for Oberon-0 that directly uses the backend API as a proof of conept of the API
- [ ] Define and implement an IR which includes type and control flow information, and a module concept based on separate symbol and object files
- [ ] Streamline and document backend API (DLL capable)
- [ ] Implement optimizer on IR level
- [ ] Implement a generator for the [Oberon+](https://github.com/rochus-keller/Oberon) frontend compatible with the IR
- [ ] Backport new TCC features if need be

#### Status on January 30, 2024

The source code of TCC 0.9.27 was downloaded from [here](http://download.savannah.gnu.org/releases/tinycc/tcc-0.9.27.tar.bz2) on 2024-01-15, and then fully decomposed, reorganized and refactored over a period of two weeks. The preprocessor, immediate execution (-run), the C67 backend, and many other features not used for the purpose were removed; the code and data structures have been trimmed accordingly. Mutual dependencies have been resolved and the code modularized so that a minimal API is now available for the backend that covers all platform specifics. The code still has a high number of global variables and gotos, and further structuring will be done where necessary (reentrancy is no project goal so far). There is a (temporary) minimized and refactored C frontend using the backend API, and working executables can be demonstrably generated (with and without debug information, using the musl runtime library), so far on Linux i386.

#### Status on February 1, 2024

The Oberon-0 parser is implemented (reusing the C compiler driver). Files can successfully be parsed. Code generator is pending.

#### Precompiled versions

Not available at this time.

#### How to build

Qt Creator 3.x is currently used for development. There are *.pro files which are compatible with qmake, but there is no Qt dependency otherwise.

#### Additional Credits

- Copyright (c) 2001-2004 Fabrice Bellard
- Copyright (c) 2003 Daniel Glöckner
- Copyright (c) 2008 Shinichiro Hamaji 
- Copyright (c) 2012 Thomas Preud'homme
- Copyright (c) 2014-2015 Edmund Grimley Evans
- [and many others](https://lists.nongnu.org/archive/html/tinycc-devel/2017-12/msg00015.html)



