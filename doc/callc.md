# Calling conventions

uLisp needs to be able to distinguish between several different kinds of
functions in order to operate. This information is stored both explicitly and
implicitly. For user-defined, interpreted functions it is implicit – there’s
only one type of them. (So far. Macros may yet arrive.) For user-defined,
assembly language functions it is explicit – they are their own data type. The
built-in compiled functions present a larger issue – there are three different
kinds of functions, which need to be called in a different way. In addition, the
lookup table stores information about values that aren’t functions, and the
interpreter also needs to know how to deal with those.

The solution used is to store this information in the main lookup table, as a
single byte. For user-level functions that operate like the interpreted
functions – that is, those whose argument list is evaluated in full before they
are called themselves – this byte also encodes information about how many
arguments are allowed. As represented in hexadecimal, their first digit
represents how many arguments are _required_ (a minimum) while the second digit
represents how many are _allowed_ (a maximum). The minimum value is never higher
than 7, and a maximum value of 15 means there is no upper limit.

Other kinds of values stored in the lookup table have the high-order bit set,
which would otherwise indicate eight or more minimum arguments to a function. At
present, there are only four such values:

- `#x80` for self-evaluating symbols like `t` and `nil`
- `#x81` for special operators, whose arguments are passed unevaluated
- `#x82` for tail forms, a form of special operator treated specially by the
  interpreter
- `#x83` for keywords, self-evaluating symbols that can also represent constant
  values from the C world

It is possible and likely that this representation scheme will later be changed
in order to accommodate new features.
