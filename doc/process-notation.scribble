#lang scribble/manual

@(require "def-with-nolink.rkt")

@title{Process Notation}
Scsh has a notation for controlling Unix processes that takes the form of s-expressions; this
notation can then be embedded inside of standard Scheme code. The basic elements of this notation
are @emph{process forms}, @emph{extended process forms}, and @emph{redirections}.

@section{Extended Process Forms and I/O Redirections}
An @emph{extended process form} is a specification of a Unix process to run, in a particular I/O
environment:

@codeblock{epf ::= (pf redir-1 ... redir-n)}

where @code{pf} is a process form and the @code{redir}s are redirection specs.

A @emph{redirection spec} is one of:

@tabular[#:sep @hspace[1]
         (list (list @code{(< [fdes] file-name)} "Open file for read.")
               (list @code{(> [fdes] file-name)} "Open file create/truncate.")
               (list @code{(<< [fdes] object)} @para{Use @code{object}'s printed rep.})
               (list @code{(>> [fdes] file-name)} "Open file for append.")
               (list @code{(= [fdes] fdes/port)} "Dup2")
               (list @code{(- fdes/port)} @nested{Close @code{fdes/port}})
               (list @code{stdports} "0,1,2 dup'd from standard ports."))]

The input redirections default to file descriptor 0; the output redirections default to file
descriptor 1.

The subforms of a redirection are implicitly backquoted, and symbols stand for their print-names.
So @code{(> ,x)} means ``output to the file named by Scheme variable @code{x},'' and
@code{(< /usr/shivers/.login)} means ``read from  @code{/usr/shivers/.login}.''

Here are two more examples of I/O redirection:

@codeblock{(< ,(vector-ref fv i)) 
           (>> 2 /tmp/buf)}

These two redirections cause the file @code{fv[i]} to be opened on stdin, and @code{/tmp/buf} to be
opened for append writes on stderr.

The redirection @code{(<< object)} causes input to come from the printed representation of
@code{object}. For example,
         
@codeblock{(<< "The quick brown fox jumped over the lazy dog.")}

causes reads from stdin to produce the characters of the above string. The object is converted to
its printed representation using the @code{display} procedure, so

@codeblock{(<< (A five element list))}

is the same as

@codeblock{(<< "(A five element list)")}

is the same as

@codeblock{(<< ,(reverse '(list element five A)))}

(Here we use the implicit backquoting feature to compute the list to be printed.)

The redirection @code{(= fdes fdes/port)} causes @code{fdes/port} to be dup'd into file descriptor
@code{fdes}. For example, the redirection

@codeblock{(= 2 1)}

causes stderr to be the same as stdout. @code{fdes/port} can also be a port, for example:

@codeblock{(= 2 ,(current-output-port))}

causes stderr to be dup'd from the current output port. In this case, it is an error if the port is
not a file port (e.g., a string port).

More complex redirections can be accomplished using the @code{begin} process form, discussed below,
which gives the programmer full control of I/O redirection from Scheme.


@subsection{Port and File Descriptor Sync}
It's important to remember that rebinding Scheme's current I/O ports (e.g., using
@code{call-with-input-file} to rebind the value of @code{(current-input-port)}) does @emph{not}
automatically ``rebind'' the file referenced by the Unix stdio file descriptors 0, 1, and 2.
This is impossible to do in general, since some Scheme ports are not representable as Unix file
descriptors. For example, many Scheme implementations provide ``string ports,'' that is, ports that
collect characters sent to them into memory buffers. The accumulated string can later be retrieved
from the port as a string. If a user were to bind @code{(current-output-port)} to such a port, it
would be impossible to associate file descriptor 1 with this port, as it cannot be represented in
Unix. So, if the user subsequently forked off some other program as a subprocess, that program
would of course not see the Scheme string port as its standard output.


To keep stdio synced with the values of Scheme's current I/O ports, use the special redirection
@code{stdports}. This causes 0, 1, 2 to be redirected from the current Scheme standard ports. It is
equivalent to the three redirections:

@codeblock{(= 0 ,(current-input-port))
           (= 1 ,(current-output-port))
           (= 2 ,(error-output-port))}

The redirections are done in the indicated order.  This will cause an error if one of the current
I/O ports isn't a Unix port (e.g., if one is a string port). This Scheme/Unix I/O synchronisation can
also be had in Scheme code (as opposed to a redirection spec) with the @code{(stdports->stdio)}
procedure.

@section{Process Forms}
A @emph{process form} specifies a computation to perform as an independent Unix process. It can be
one of the following:

@codeblock{(begin . scheme-code)            ; Run scheme-code in a fork.
           (| pf-1 ... pf-n)                ; Simple pipeline
           (|+ connect-list pf-1 ... pf-n)  ; Complex pipeline
           (epf . epf)                      ; An extended process form.
           (prog arg-1 ... arg-n)           ; Default: exec the program.}

The default case @code{(prog arg-1 ... arg-n)} is also implicitly backquoted. That is, it is
equivalent to:

@codeblock{(begin (apply exec-path `(prog arg-1 ... arg-n)))}

@code{exec-path} is the version of the
@hyperlink["http://www.FreeBSD.org/cgi/man.cgi?query=exec&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html"]{@code{exec()}}
system call that uses scsh's path list to search for an executable. The program and the arguments
must be either strings, symbols, or integers. Symbols and integers are coerced to strings. A
symbol's print-name is used. Integers are converted to strings in base 10. Using symbols instead of
strings is convenient, since it suppresses the clutter of the surrounding @code{"..."} quotation
marks. To aid this purpose, scsh reads symbols in a case-sensitive manner, so that you can say

@codeblock{(more Readme)}

and get the right file.

A @code{connect-list} is a specification of how two processes are to be wired together by pipes. It
has the form @code{((from-1 from-2 ... to) ...)} and is implicitly backquoted. For example,

@codeblock{(\|+ ((1 2 0) (3 1)) pf-1 pf-2)}
@; fix the need for \

runs @code{pf-1} and @code{pf-2}. The first clause @code{(1 2 0)} causes @code{pf-1}'s stdout (1)
and stderr (2) to be connected via pipe to @code{pf-2}'s stdin (0). The second clause @code{(3 1)}
causes @code{pf-1}'s file descriptor 3 to be connected to @code{pf-2}'s file descriptor 1.

The @code{begin} process form does a @code{stdio->stdports} synchronisation in the child process
before executing the body of the form. This guarantees that the @code{begin} form, like all other
process forms, ``sees'' the effects of any associated I/O redirections.

Note that RnRS does not specify whether or not @exec{|} and @exec{|+} are readable symbols. Scsh
does.

@section[#:tag "using-extended-process-forms-in-scheme"]{Using Extended Process Forms in Scheme}
Process forms and extended process forms are @emph{not} Scheme. They are a different notation for
expressing computation that, like Scheme, is based upon s-expressions. Extended process forms are
used in Scheme programs by embedding them inside special Scheme forms. There are three basic Scheme
forms that use extended process forms: @code{exec-epf}, @code{&}, and @code{run}.

@codeblock{(exec-epf . epf) ; => no return value (syntax)
           (& . epf)        ; => proc (syntax)
           (run . epf)      ; => status (syntax)}

The @code{(exec-epf . epf)} form nukes the current process: it establishes the I/O redirections and
then overlays the current process with the requested computation.

The @code{(& . epf)} form is similar, except that the process is forked off in background. The form
returns the subprocess' process object.

The @code{(run . epf)} form runs the process in foreground: after forking off the computation, it
waits for the subprocess to exit, and returns its exit status.

These special forms are macros that expand into the equivalent series of system calls. The
definition of the @code{exec-epf} macro is non-trivial, as it produces the code to handle I/O
redirections and set up pipelines.

However, the definitions of the @code{&} and @code{run} macros are very simple:

@tabular[#:sep @hspace[1]
         (list (list @code{(& . epf)} @code{(fork (lambda () (exec-epf . epf)))})
               (list @code{(run . epf)} @code{(wait (& . epf))}))]

@subsection{Procedures and Special Forms}
It is a general design principle in scsh that all functionality made available through special
syntax is also available in a straightforward procedural form. So there are procedural equivalents
for all of the process notation. In this way, the programmer is not restricted by the particular
details of the syntax.

Here are some of the syntax/procedure equivalents:

@tabular[#:sep @hspace[1]
         (list (list @exec{|} @code{fork/pipe})
               (list @exec{|+} @code{fork/pipe+})
               (list @code{exec-epf} @code{exec-path})
               (list "redirection" @nested{@code{open}, @code{dup}})
               (list @code{&} @code{fork})
               (list @code{run} @nested{@code{exec} + @code{fork}}))]

Having a solid procedural foundation also allows for general notational experimentation using
Scheme's macros. For example, the programmer can build his own pipeline notation on top of the
@code{fork} and @code{fork/pipe} procedures.

@;@ref[chapt:syscalls] fix this ref
gives the full story on all the procedures in the syscall library.

@subsection{Interfacing Process Output to Scheme}
There is a family of procedures and special forms that can be used to capture the output of
processes as Scheme data. These forms all fork off subprocesses, collecting the process' output to
stdout in some form or another. The subprocess runs with file descriptor 1 and the current output
port bound to a pipe. Furthermore, each of these forms is a simple expansion into calls to analogous
procedures. For example, @code{(run/port . epf)} expands into
@code{(run/port* (lambda () (exec-epf . epf)))}.

@deftogether[(@defform/nolink[(run/port epf)]
              @defproc/nolink[(run/port* [thunk (-> any/c)]) port?])]{
Returns a port open on process's stdout. Returns immediately after forking child process.
}

@deftogether[(@defform/nolink[(run/file epf)]
              @defproc/nolink[(run/file* [thunk (-> any/c)]) string?])]{
Returns the name of a temp file containing the process's output. Returns when the process exits.
}

@deftogether[(@defform/nolink[(run/string epf)]
              @defproc/nolink[(run/string* [thunk (-> any/c)]) string?])]{
Returns a string containing the process' output. Returns when an eof is read.
}

@deftogether[(@defform/nolink[(run/strings epf)]
              @defproc/nolink[(run/strings* [thunk (-> any/c)]) (listof string?)])]{
Splits process' output into a list of newline-delimited strings. The delimiting newlines are not
part of the returned strings. Returns when an eof is read.
}

@deftogether[(@defform/nolink[(run/sexp epf)]
              @defproc/nolink[(run/sexp* [thunk (-> any/c)]) any/c])]{
Returns a single object from process' stdout with @code{read}. Returns as soon as the read
completes.
}

@deftogether[(@defform/nolink[(run/sexps epf)]
              @defproc/nolink[(run/sexps* [thunk (-> any/c)]) (listof any/c)])]{
Repeatedly reads objects from process' stdout with @code{read}. Returns accumulated list upon eof.
}

@subsubsection{Parsing Input from Ports}
The following procedures are also of utility for generally parsing input streams in scsh:

@defproc/nolink[(port->string [port port?]) string?]{
Reads the port until eof, then returns the accumulated string.
}

@defproc/nolink[(port->sexp-list [port port?]) (listof any/c)]{
Repeatedly reads data from the port until eof, then returns the accumulated list of items.
}

@defproc/nolink[(port->string-list [port port?]) (listof string?)]{
Repeatedly reads newline-terminated strings from the port until eof, then returns the accumulated
list of strings. The delimiting newlines are not part of the returned strings.
}

@defproc/nolink[(port->list [reader (-> port? any/c)] [port port?]) (listof any/c)]{
Generalises @code{port->sexp-list} and @code{port->string-list}. Uses @code{reader} to repeatedly
read objects from a port and accumulates these objects into a list, which is returned upon eof.
@code{port->sexp-list} and @code{port->string-list} are trivial to define, being merely
@code{port->list} curried with the appropriate parsers:

@codeblock{(port->string-list port) => (port->list read-line port)
           (port->sexp-list   port) => (port->list read port)}

The following compositions also hold:

@codeblock{run/string* => (compose port->string run/port*)
           run/strings* => (compose port->string-list run/port*)
           run/sexp* => (compose read run/port*)
           run/sexps* => (compose port->sexp-list run/port*)}
}


@defproc/nolink[(port-fold [port port?]
                                      [reader (-> port? any/c)]
                                      [op (-> any/c any/c ...+ (values any/c ...+))]
                                      [seeds any/c] ...+) (values any/c ...+)]{
This procedure can be used to perform a variety of iterative operations over an input stream. It
repeatedly uses @code{reader} to read an object from @code{port}. If the first read returns eof,
then the entire @code{port-fold} operation returns the seeds as multiple values.

If the first read operation returns some other value @code{v}, then @code{op} is applied to @code{v}
and the seeds: @code{(op v . seeds)}. This should return a new set of seed values, and the reduction
then loops, reading a new value from the port, and so forth. If multiple seed values are used, then
@code{op} must return multiple values.

For example,

@codeblock{(port->list reader port)}

could be defined as

@codeblock{(reverse (port-fold port reader cons '()))}

An imperative way to look at @code{port-fold} is to say that it abstracts the idea of a loop over a
stream of values read from some port, where the seed values express the loop state.
}

@section{More Complex Process Operations}
The procedures and special forms in @secref["using-extended-process-forms-in-scheme"] provide for the
common case, where the programmer is only interested in the output of the process. These special
forms and procedures provide more complicated facilities for manipulating processes.

@subsection{Pids and Ports Together}

@deftogether[(@defform/nolink[(run/port+proc epf)]
              @defproc/nolink[(run/port+proc* [thunk (-> any/c)]) (values [port port?] [proc process?])])]{
This special form and its analogous procedure can be used if the programmer also wishes access to
the process' pid, exit status, or other information. They both fork off a subprocess, returning two
values: a port open on the process' stdout (and current output port), and the subprocess's process
object. A process object encapsulates the subprocess' process id and exit code; it is the value
passed to the @code{wait} system call.
@; make the description of process objects a reference

For example, to uncompress a tech report, reading the uncompressed data into scsh, and also be able
to track the exit status of the decompression process, use the following:

@codeblock{(receive (port child) (run/port+proc (zcat tr91-145.tex.Z))
             (let* ((paper (port->string port))
                    (status (wait child)))
               ... use paper, status, and child here ...))}

Note that you must @emph{first} do the @code{port->string} and @emph{then} do the wait---the other
way around may lock up when the zcat fills up its output pipe buffer.
}

@subsection{Multiple Stream Capture}
Occasionally, the programmer may want to capture multiple distinct output streams from a process.
For instance, he may wish to read the stdout and stderr streams into two distinct strings. This is
accomplished with the @code{run/collecting} form and its analogous procedure,
@code{run/collecting*}.

@deftogether[(@defform/nolink[(run/collecting fds ...+ epf)]
              @defproc/nolink[(run/collecting* [fds (listof integer?)]
                                                          [thunk (-> any/c)])
                       (values [status integer?] [port port?] ...+)])]
These guys run processes that produce multiple output streams and return ports open on these
streams. To avoid issues of deadlock, @code{run/collecting} doesn't use pipes. Instead, it first
runs the process with output to temp files, then returns ports open on the temp files. For example,

@codeblock{(run/collecting (1 2) (ls))}

runs @code{ls} with stdout (fd 1) and stderr (fd 2) redirected to temporary files. When the
@code{ls} is done, @code{run/collecting} returns three values: the @code{ls} process' exit status,
and two ports open on the temporary files. The files are deleted before @code{run/collecting}
returns, so when the ports are closed, they vanish. The @code{fds} list of file descriptors is
implicitly backquoted by the special-form version.

For example, if Kaiming has his mailbox protected, then

@codeblock{(receive (status out err)
                    (run/collecting (1 2) (cat /usr/kmshea/mbox))
             (list status (port->string out) (port->string err)))}

might produce the list

@codeblock{(256 "" "cat: /usr/kmshea/mbox: Permission denied")}

What is the deadlock hazard that causes @code{run/collecting} to use temp files? Processes with
multiple output streams can lock up if they use pipes to communicate with Scheme I/O readers. For
example, suppose some Unix program @code{myprog} does the following:

@itemlist[#:style 'ordered
          @item{First, outputs a single ``@exec{(}'' to stderr.}
          @item{Then, outputs a megabyte of data to stdout.}
          @item{Finally, outputs a single ``@exec{)}'' to stderr, and exits.}]

Our scsh programmer decides to run @code{myprog} with stdout and stderr redirect
@emph{via Unix pipes} to the ports @code{port1} and code{port2}, respectively. He gets into trouble
when he subsequently says @code{(read port2)}. The Scheme @code{read} routine reads the open paren,
and then hangs in a
@hyperlink["http://www.FreeBSD.org/cgi/man.cgi?query=read&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html"]{read()}}
system call trying to read a matching close paren. But before @code{myprog} sends the close paren
down the stderr pipe, it first tries to write a megabyte of data to the stdout pipe. However, Scheme
is not reading that pipe---it's stuck waiting for input on stderr. So the stdout pipe quickly fills
up, and @code{myprog} hangs, waiting for the pipe to drain. The @code{myprog} child is stuck in a
stdout/@code{port1} write; the Scheme parent is stuck in a stderr/@code{port2} read. Deadlock.

@; ^might not be a problem in modern s48. doesn't use blocking read? research.

Here's a concrete example that does exactly the above:

@codeblock{(receive (status port1 port2)
                    (run/collecting (1 2) 
             (begin
               ;; Write an open paren to stderr.
               (run (echo "(") (= 1 2))
               ;; Copy a lot of stuff to stdout.
               (run (cat /usr/dict/words))
               ;; Write a close paren to stderr.
               (run (echo ")") (= 1 2))))

   ;; OK. Here, I have a port PORT1 built over a pipe
   ;; connected to the BEGIN subproc's stdout, and
   ;; PORT2 built over a pipe connected to the BEGIN
   ;; subproc's stderr.
   (read port2) ; Should return the empty list.
   (port->string port1)) ; Should return a big string.
}

In order to avoid this problem, @code{run/collecting} and @code{run/collecting*} first run the child
process to completion, buffering all the output streams in temp files (using the
@code{temp-file-channel} procedure). When the child process exits, ports open on the buffered output
are returned. This approach has two disadvantages over using pipes:

@; link to temp-file-channel definition

@itemlist[@item{The total output from the child output is temporarily written to the disk before
                returning from @code{run/collecting}. If this output is some large intermediate
                result, the disk could fill up.}
          @item{The child producer and Scheme consumer are serialised; there is no concurrency
                overlap in their execution.}]

However, it remains a simple solution that avoids deadlock.  More sophisticated solutions can easily
be programmed up as needed---@code{run/collecting*} itself is only 12 lines of simple code.

See @code{temp-file-channel} for more information on creating temp files as communication channels.

@section{Conditional Process Sequencing Forms}
These forms allow conditional execution of a sequence of processes.

@defform/nolink[(|| pf ...+)]{
Run each proc until one completes successfully (i.e., exit status zero). Return true if some proc
completes successfully; otherwise @code{#f}.
}

@defform/nolink[(&& pf ...+)]{
Run each proc until one fails (i.e., exit status non-zero). Return true if all procs complete
successfully; otherwise @code{#f}.
}

@section{Process Filters}
These procedures are useful for forking off processes to filter text streams.

@defproc/nolink[(make-char-port-filter [filter (-> character? character?)]) procedure?]{
Returns a procedure that when called, repeatedly reads a character from the current input port,
applies @code{filter} to the character, and writes the result to the current output port. The
procedure returns upon reaching eof on the input port.

For example, to downcase a stream of text in a spell-checking pipeline, instead of using the Unix
@code{tr A-Z a-z} command, we can say:

@codeblock{(run (\| (delatex)
                   (begin ((char-filter char-downcase))) ; tr A-Z a-z
                   (spell)
                   (sort)
                   (uniq))
                (< scsh.tex)
                (> spell-errors.txt))}
}

@defproc/nolink[(make-string-port-filter [filter (-> string? string?)]
                                                    [buflen integer? 1024]) procedure?]{
Returns a procedure that when called, repeatedly reads a string from the current input port, applies
@code{filter} to the string, and writes the result to the current output port. The procedure returns
upon reaching eof on the input port.

The optional @code{buflen} argument controls the number of characters each internal read operation
requests; this means that @code{filter} will never be applied to a string longer than @code{buflen}
chars.
}