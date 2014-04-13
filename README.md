Scsh
====
Scsh is a unix shell embedded in Scheme. What does that mean? Well, unix shells are powerful
tools. They allow a user to concisely specify her commands and the communications between them
(piping, redirecting, &c.). When she needs to do something more complex than running a set of
commands with known inputs, however, things become complicated. General programming with sh can be
unpleasant and error prone, to say the least.

Scheme is a simple, expressive general programming language. A user with some taste may want to use
it to wield her computing machine. For simple commands, however, it is not the most concise. At the
scale of the command line, even the overhead of parentheses matters. It would be nice to use each of
these languages where their strengths lie.

Scsh is the solution. It allows the user to write commands in a language within Scheme that follows
the unix way, but also allows her to specify more complex commands with the elegance of Scheme.

Getting Started
===============

Dependencies
------------
In order to run scsh, you'll need to be running on a POSIX complient operating system, the gnu build
system, and scheme48 1.9.2

Installing scheme48 1.9.2
-------------------------
If scheme48 1.9.2 is not available in a package manager for your operating system, follow the
instructions for installing from the source distribution here <http://www.s48.org/1.9.2/download.html>.

Building scsh
-------------
Within a fresh checkout of scsh, run the following commands

    $ git submodule update --init
    $ autoreconf
    $ ./configure
    $ make

That'll pull all the scheme dependencies and build scsh. To try scsh before installing, run `./go`
from that same directory. That'll open up a repl with standard r5rs scheme in the environment,
scsh's command language, and a large posix library (docs to come). To run scsh's test suite, run
`make test`.

Installing scsh
---------------
Run `make install` to install scsh on your system. Standard gnu build system rules apply. So, for
instance, if you'd like to install to a location other than the default (usually `/usr/local/`),
rerun `./configure` the appropriate flags.
