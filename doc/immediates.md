# Immediate values

Like other Lisps, but unlike languages like C, uLisp attaches type information
to all of its runtime objects. Originally, this was done in a fairly
straightforward and uniform fashion: All objects were represented by two values
of the same size as the underlying machine’s memory pointers. Cons cells, which
always point to other values, had actual pointers in both cells, while other
kinds of objects had a type tag in the `car` cell and a representation of the
object in the `cdr` cell. This representation is simple to understand, but it is
wasteful: To represent an n-bit number we use two n-bit values! There is,
however, a way to increase the efficiency of this scheme for cases that warrant
the effort but retain most of the simplicity where it isn’t.

Notice that even in the simple version we need to have an infallible way to
distinguish between type tags and memory pointers. This turns out to be possible
in very common cases due to alignment constraints. To take an example, if we
ensure that our table of uLisp objects _starts_ at an even memory address, no
valid pointer to a uLisp object will ever be an odd number – even on 8-bit
machines, pointers are always at least two bytes. As it happens, uLisp _already_
uses the lowest bit – the one controlling whether a number is even or odd – for
[garbage collection](http://www.ulisp.com/show?1BD3). That’s fine by us:
Instead, we ensure that the table starts at a multiple of four. You see, a uLisp
object is two pointers, so if the first address is a multiple of four, all of
them are. This enables us to use the other bits to represent objects in a way
that doesn’t require memory to be allocated. Instead of representing n-bit
numbers with 2n bits we can now represent numbers with n-2 bits in n
bits. That’s considerably less wasteful, and all we have to do to know that it
_is_ a number is to check if the bit signifying 2 is set. Great!

There is one catch: Remember those type tags from earlier? We still need those
for our remaining boxed values. Therefore, in order to properly gain, we can’t
just stuff numbers into the n-2 bits. We have to be able, again, to tell type
tags and numbers apart with certainty. We’ll have to sacrifice an additional
bit. Having done that, to figure out whether the value is a number or a type
tag, we need to look at two bits, those representing 6. Since it’s two bits,
there are four possible combinations, but two of those – 0 and 4 – represent a
pointer, not an immediate value. Let’s say that if the value is 2, we have a
number, and if it’s 6 we have a type tag. Then we have n-3 (13, 16, 61) bit
numbers, which seems reasonable. But now a question arises - do we need that
many bits to distinguish between types?

Well, no. And we might want to stick other kinds of values into immediates –
like, say, character values. If using two 16-bit values to represent a 16-bit
number felt wasteful, using those same 32 bits to represent an 8-bit value is
even worse. So we extend this system: Ignoring the lowest-order bit, an
immediate object’s type is identified by the first unset bit. This means we can
check types with fairly simple bitmasks – all the values used are powers of two,
minus two.

## Implemented immediate types

uLisp has implementations that use 16-bit, 32-bit and 64-bit pointers. The
differences in size mean that some aspects of the implementation differ between
platforms. In particular, it is possible to encode user-defined symbols in an
immediate value on the larger machines, which is not reasonable for 16-bit
pointers. For this reason, the immediate types do vary by bit-size. They do not
vary by anything else, however. Certain fundamentals are shared - fixnums always
have the same tag, for example.

### 16-bit

- Fixnums are thirteen-bit signed integers. `(eql (logand fixnum 6) 2)`
- Characters are eight-bit unsigned integers. `(eql (logand byte 254) 126)`

### 32-bit

- Fixnums are 29-bit signed integers. `(eql (logand fixnum 6) 2)`
- Symbols are packed into 27 bits. `(eql (logand symbol 30) 14)`
- Characters are 21-bit unsigned integers. `(eql (logand unicode 2046) 1022)`

### 64-bit

- Fixnums are 61-bit signed integers. `(eql (logand fixnum 6) 2)`
- Symbols are packed into 59 bits. `(eql (logand symbol 30) 14)`

## Possible extensions

### Parametric types

The type tags, especially on the larger platforms, are _much_ larger than they
need themselves. It is possible to specify a fixed number of bits to be used for
the type tag itself, and allocate the rest of the bits to some kind of
parameters for the type. A potential use for the platforms that could feasibly
run their own compiler would be to stash calling convention information about
functions in there.

It might also, by complicating the memory allocation mechanism a little, be used
to carry size information for contiguous allocations larger than a single cell.
