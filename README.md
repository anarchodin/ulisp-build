# uLisp

[uLisp](http://www.ulisp.com) is a version of the Lisp programming language,
primarily targeting microcontrollers. See the website for more details.

You have stumbled across a reworking of the core source of uLisp. This
repository contains two main components: The (mostly) C code that gets
concatenated into Arduino sketches for uLisp releases, and the Common Lisp code
that handles this concatenation (and some automatic code generation).

The two are in the process of being teased apart. Once that's further underway,
there'll be better documentation here.

## Usage

The build system is written in Common Lisp, and is being distributed as source
code. Its use therefore requires an implementation of Common Lisp to be
available. My main development environment is based on [SBCL](http://sbcl.org),
but I have tried to ensure it also works on the quite portable
[ECL](https://common-lisp.net/project/ecl/). The code is mostly portable Common
Lisp, but it expects to be loaded using ASDF and therefore uses UIOP
functions. As far as I can tell, all actively-developed implementations of
Common Lisp provide these. In theory, any of them should work.

There is an ASDF system definition here, so one way of running the system is by
getting ASDF to find that and doing `(asdf:load-system :ulisp-build)`. All the
code is in the `ulisp-build` package. It currently does weird things if that
isn't the current package, which can probably be fixed with the right printer
control variables.

There's also the file `run-generator.lisp` file, which configures ASDF to search
the current directory, loads the system, builds uLisp and (in ECL and ABCL)
exits. A simple shell script that loads this file in ECL is in `build-all.sh`.

I will readily concede that this interface is not very inviting to those who are
unused to working on the command line and/or with Common Lisp. I am open to
suggestions towards improving that.

## Licensing

[uLisp's licence](http://www.ulisp.com/show?1B83) is fairly clear. The
alterations made in this repository should be considered available under the
same terms.
