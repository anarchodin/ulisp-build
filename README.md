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

The build system depends on a Common Lisp implementation. I have tested
[SBCL](http://sbcl.org) and [ECL](https://common-lisp.net/project/ecl/).

There is an ASDF system definition here, so one way of running the system is by
getting ASDF to find that and doing `(asdf:load-system :ulisp-build)`. All the
code is in the `ulisp-build` package. It currently does weird things if that
isn't the current package, which can probably be fixed with the right printer
control variables.

There's also the file `run-generator.lisp` file, which configures ASDF to search
the current directory, loads the system, builds uLisp and (in ECL) exits. An
example use, which also uses [arduino-cli][arduino] to compile uLisp for the
[Adafruit ItsyBitsy M4 Express][itsym4], is in `test.sh`.

[arduino]: https://arduino.github.io/arduino-cli/
[itsym4]: https://www.adafruit.com/product/3800

## Licencing

[uLisp's licence](http://www.ulisp.com/show?1B83) is fairly clear. The
alterations made in this repository should be considered available under the
same terms.
