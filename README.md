

# To Build

First you need to install [stack](https://github.com/commercialhaskell/stack).
If you don't want to it is already on the cs servers.

    git clone https://github.com/ethanpailes/tmi
    cd tmi
    stack build
    cp $(stack exec which tm) .

The last line is optional. It just copies the binary from where stack puts
it to your current directory.
This may take a while because stack is going to pull in a bunch of libraries.
Once you have tmi built you should be able to run in with:

    ./tm <args here>

For example

    ./tm p3.txt 0001110000

Hopefully this will print Accept to the screen. If it does not one of us
screwed up.


### Getting tm on the Tufts CS servers

    cp /h/epaile01/tm .

I may end up fixing bugs at some point in which case I will try to
update this version of the interpreter.

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


