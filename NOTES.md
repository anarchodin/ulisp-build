# Some notes

## AVR and progmem

The AVR's Harvard architecture is the root cause of several headaches, but one
in particular is hilarious: ATmega4809 _is_ an AVR, but exposes the flash memory
in the RAM address space. This means that it uses the same code as non-AVR
platforms. Doing that check in CPP every time would be stupidly noisy, so
instead we do it once, in the AVR header, and define the macro `NEEDS_PROGMEM`
for all _other_ AVRs.

The code that needs to use that macro seems like a prime target for refactoring.

## Calling conventions

The main lookup table contains a byte value which is currently used to encode
the minimum and maximum argument numbers. This is done by splitting the field in
two, with four bits going to a min value, and four to a max value. If the latter
field is all bits set, no maximum is enforced. Compact and simple.

However, there are only 23 distinct such combinations among the functions
defined now. If we are willing to drop the ‘simple’ part this can be exploited
to encode more information – and a straightforward choice is to encode calling
convention information explicitly. Such information is currently stored
implicitly in the order of the lookup table, and is the sole reason that table
needs to be in any _specific_ order. (Regardless of this, its order needs to
match up with that of the enums.) The order is maintained by several sentinel
values, which splits the table into sections:

1. __Symbols__

    The first section, not preceded by a sentinel value. The symbols here are a
    mixed bag of things useful in user code and in the implementation, but they
    evaluate to themselves and can not be used as functions. They are
    distinguished from the keywords (later) by not being associated with a C
    value.

2. __Special forms__

    Located after the first sentinel value, and fairly self-explanatory. These
    symbols have a function definition attached, but that function gets handed
    the argument list with no evaluation performed. True to history, several of
    these would be macros in a different Lisp.

3. __Tail forms__

    Located after the second sentinel value, in essence an extension of the
    previous group. These symbols name special operators that the evaluator
    replaces with their return value – sort of like a macro. This implements
    tail calls in uLisp.

4. __Functions__

    Located after the third sentinel value, and by far the largest group. These
    symbols have an associated function definition, and their arguments are
    evaluated before being passed (in a list). The evaluator verifies the
    argument count against the lookup table.

    Unlike other Lisps, functions _are_ given access to the current lexical
    environment. This should be reviewed.

5. __Keywords__

    Located after the fourth sentinel value, and treated mostly like the first
    group by the evaluator. However, the lookup table stores some additional
    data, which is used to forward constant values into C APIs. These already
    use the argument count to encode data, and thus pose a possible issue.

6. __User functions__

    Not present in this codebase, but used in mainline uLisp. Located after the
    fifth sentinel value. An empty section intended for C functions added by the
    user. Additionally followed by a sixth and final sentinel value.

It is immediately possible to fold functions and user functions together, as the
latter group exists only because of the ordering requirement. That means there
are five different kinds of entries in the table. Five types of entries, and 23
different signatures should fit into a byte quite comfortably.

Since none of the existing functions have eight or more _minimum_ arguments, and
functions with such a signature are unlikely to be added, I propose that this be
used to distinguish between argument counts and other calling convention
information: If the byte’s high bit is unset, the value is read as before and
indicates a standard function. If the high bit _is_ set, the entry does not
represent a standard function, and the value read has a different meaning. For
now, I propose having it extremely simple: `#x80` means it’s a self-evaluating
symbol, `#x81` is a special operator, `#x82` is a tail form and `#x83` is a
keyword.

To return to the rationale: If this change is made, the central lookup table no
longer has an ordering dependency. There are other potential benefits, but this
is the immediate one. Not needing to be concerned about the relative order of
groups of symbols makes it a _lot_ easier to split up the definition of the
lookup table and group the symbols by features.

In order to make this change, there are a handful of steps:

- [ ] Ensure existing argument checking logic continues to work.
- [ ] Alter keywords so they don’t use the `minmax` value.
- [ ] Move the interpreter’s handling logic from using sentinel values to the
      new system.

On both AVR and ARM, all current keywords alias values below 256. We
can therefore move the context information into the pointer field, shifting and
ORing them together.
