

# To Build

First you need to install [stack](https://github.com/commercialhaskell/stack).
If you don't want to it is already on the cs servers.

# Usage

This is pretty hacked together so I did not polish the command line
interface as much as I would have liked. Usage is as follows.

    tm <Turing machine file> <input tape> [<flag>]

Here <Turing machine file> is the name of your file. It should follow
the format described by the perl scrip provided by the course staff.
That script is the authority. My interpreter is not affiliated with
the course. <input tape> is a list of symbols in Sigma. <flag> is
one of two things.

    -t       test. runs some fuzz tests based on the langauge we are asked
              to decide
    -v       verbose. spits out each application of delta on a seperate line.
              useful for debugging a specific input.


