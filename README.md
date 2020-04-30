# uLisp

[uLisp](http://www.ulisp.com) is a version the Lisp programming language,
primarily targeting microcontrollers. See the website for more details.

You have stumbled across the core source of uLisp. This repository contains two
main components: The (mostly) C code that gets concatenated into Arduino
sketches for uLisp releases, and the Common Lisp code that handles this
concatenation (and some automatic code generation).

The two are in the process of being teased apart. Once that's further underway,
there'll be better documentation here.

## Usage

The build system depends on a Common Lisp implementation. I have tested
[SBCL](http://sbcl.org) and [ECL](https://common-lisp.net/project/ecl/).

At present slight edits to the file `Load Preprocessor.lisp` are required: Alter
the `push` to match the variant you wish to build. Under ECL, the remaining
process is fairly simple: <kbd>ecl --norc --load "Load Preprocessor.lisp" --eval
'(in-package :ulisp-build)'</kbd> loads the required code. From there, call the
function `generate`, with a keyword representing the variant desired.

The code is also packaged using ASDF, as the system `ulisp-build`.

## Licencing

The C code has (mostly) been published under MIT licence by David
Johnson-Davies. The Common Lisp code has _not_, and was supplied privately to
Herbert Snorrason just prior to the release of uLisp 3.2 in April 2020.

The repository will remain private until its publication has been cleared by the
original author.
