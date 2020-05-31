# Pinary - Printable Binary

A marginally useful library for converting binary, or any number really, into 
printable text!

```haskell
encode base26 $ fromBase10 3276872
-- "hello"

encode base94 $ decode base26 "hello"
-- "$yqA"
```

## How it Works

Binary data is really just one really long number. This library takes exploits that
fact allowing you to convert binary data into various different representations
just by changing the radix of its numerical value.

This is done by converting a number (or equivalently, some binary data) into a 
sequence of digits. For example, the `Data.Radix` module provides the `fromBase10`
function allowing you to convert a regular integer into it's digit representation:

```haskell
fromBase10 42
-- Digits {radix = 10, digits = [4,2]}
```

Once in its digit representation, the value can be freely converted to a different
radix using `convertRadix`:

```haskell
convertRadix 16 $ fromBase10 42
-- Digits {radix = 16, digits = [2,10]}
```

That's all well and good, but it is a bit difficult to tell, at first glance, that
the digits `[2, 10]` really mean the hexadecimal number `0x2a`. To convert a digit
representation into something (barely) readable by a human we need to encode it
using an `Alphabet`.

The `Alphabet` type (defined in `Data.Alphabet`) defines which tokens are used to
represent the cardinal value of a digit. To convert a digit representation into
some actual text we can use the `encode` function:

```haskell
encode hexadecimal . convertRadix 16 . fromBase10 $ 42
-- "2a"
```

The `encode` function internally converts the radix of the input digit representaiton
to match that of the supplied alphabet so the above can be simplified to:

```haskell
encode hexadecimal $ fromBase10 42
-- "2a"
```

Of course there is also a `decode` function to convert a sequence of letters into a
digit representation, as well as `toBase10` to convert back to an integer:

```haskell
toBase10 . decode hexadecimal $ "2a"
-- 42
```

Here, `hexadecimal` is a predefined alphabet defined in `Data.Alphabet`. There are
a bunch of other alphabets defined in there including the interesting `base94`
alphabet:

```haskell
> encode base94 $ fromBase10 95123584511
"-{C.ed"
```

There are 94 printable ASCII characters not including whitespace characters so
`base94` is the largest possible base where all the digits are printable and
take up a single byte each of memory.

## Is This Useful?

Short answer: probably not.

The one potential use case for this type of program would be to convert binary
data into a compact form which is transmitable by services which only accept
printable bytes. One could, theoretically, use this to transmit binary data
via text message without having to use MMS.
