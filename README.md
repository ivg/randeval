# Fuzzes a sequence of bytes

This program evaluates one or more instruction sequences picking
inputs from the specified (but currently hardcoded) set of values.

Example,

```
./randeval.native eval "\x48\x8d\x74\x24\x30"
{
  RSI := low:64[RSP + 0x30]
}
Input: RSP
State 0: [0]
    RSI => 0x30
State 1: [1]
    RSI => 0x31
State 2: [0xAAAAAAAAAAAAAAAA]
    RSI => 0xAAAAAAAAAAAAAADA
State 3: [0xFFFFFFFFFFFFFFFF]
    RSI => 0x2F

```

Each state is defined by an n-tuple, where `n` is the total number of
input variables in the given sequence of bytes, and the total number
of states investigated is equal to `k^n`, where `k` is the total
number of seeds. By default, we use 4 seeds, `0`, `1`, `0xAA..`, and
`0xFF..`.

## The `eval` command

This command will print the BIL representation of each provided
sequence of bytes, and evaluate them in order. For each state, it will
print the final environment. Note, all side-effects are represented as
variables in the environemnt, e.g., jumps a represented as assignments
to the `PC` variable.

## The `diff` command

This command will treat the first sequence of bytes as the base one
and then compare the rest sequences with it. The comparison is made
state by state, and the differences between states are printed.

Note, the results of evaluation of the first instruction are memoized,
enabling an efficient comparison of one instruction to many
candidates.

Example:
```
./randeval.native diff "\x48\x8d\x74\x24\x30" \
                       "\x48\x8d\x74\x30\x24" \
                       "\x48\x8d\x74\x24\x30"

{
  RSI := low:64[RSP + 0x30]
}
{
  RSI := low:64[RAX + RSI + 0x24]
}
State 0 differs:
  | RSI = 0x30 <> 0x24

State 1 differs:
  | RSI = 0x31 <> 0x25

State 2 differs:
  | RSI = 0xAAAAAAAAAAAAAADA <> 0xAAAAAAAAAAAAAACE

State 3 differs:
  | RSI = 0x2F <> 0x23

{
  RSI := low:64[RSP + 0x30]
}
State 0 matches!
State 1 matches!
State 2 matches!
State 3 matches!
```

## The `comp` command

This command is similar to `diff`, except that instead of printing the
results of comparison of each state, it will search for the first
mismatching step and print `DIFFERS` if one is found, or `MATCHES`
otherwise, and will stop afterwards. E.g.,
```
randeval.native comp "\x48\x8d\x3b" "\x48\x89\xdf" "\x48\x8b\x3b"
\x48\x89\xdf: MATCHES
\x48\x8b\x3b: DIFFERS
```
