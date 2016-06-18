## Puzzle
Generate random arithmetic expressions, that have `N` literals, each literal is integer and does not exceed `M` in absolute value. Expression evaluation result should not exceed `M` in absolute value as well.

Allowed arithmetic operations: add, subtraction, multiplication, division. Operations have usual precedence and left associativity.

### Examples
|N|M|Expression|
|-|-|-|
|6|100|(5 + 13)/(-2 + 1*(4 - 3)|
|10|20|(-4-4+3/(-4))/18*(-15)/(18*17/3-14)|

### Goals
- Implementation should read `N` and `M` from file and/or command line arguments.
- Expression should not contain redundant parentheses. For instance, `((5 + (6 * 7))` should actually be `5 + 6 * 7`, whereas `5 * (-2)` has no redundant parentheses.
- Generation should be comprehensive.

## How to run
1. Install `stack` + `ghc` or `libtinfo5`
2. Run `stack install` in project root to compile and copy binary file to stack local bin storage (by default: `~/.local/bin`)
   
   Useful commands:
   - `stack build` - compile only
   - `stack ghci` - run REPL with project modules
   - `stack test` - run unit tests
3. Run `exprgen --help` for further instructions
   
   Example: `exprgen -n 10 -m 20 -s 1`