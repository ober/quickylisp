fare-quasiquote — a pattern-matching friendly implementation of Quasiquote
==========================================================================

Copyright ⓒ 2002-2014 Fahree Reedaw <fare@tunes.org>


Purpose
-------

The main purpose of this n+2nd reimplementation of quasiquote,
is enable matching of quasiquoted patterns, using
[OPTIMA](https://github.com/m2ym/optima/).

Now, developing this implementation was also a challenge in understanding
the ins and outs of quasiquotation, and in exploring the way it interacts
with extended support for constant-building syntactic constructs.
And this, at least to me was as much fun as it was an intellectual challenge.


How to use it
-------------

The FARE-QUASIQUOTE system is a straightforward reimplementation of quasiquote;
however, it expands into a stable format that allows for pattern matching,
by privileging LIST before CONS before LIST* before APPEND before NCONC
(and BTW, you should never, ever, use NCONC).
When using FARE-QUASIQUOTE-OPTIMA, expressions parsed by FARE-QUASIQUOTE
can be used as pattern matching patterns with OPTIMA.

We recommend you use NAMED-READTABLES to enable and disable FARE-QUASIQUOTE
at the beginning and end of your Lisp files,
or around their compilation (e.g. using ASDF around-compile hooks).
Indeed, it is important not to let such READTABLE leak into
the compilation of files that do not depend on FARE-QUASIQUOTE.

Note that since OPTIMA doesn't support backtracking,
it cannot match APPEND patterns, and those quasiquote templates that
expand into something using APPEND can't be used as patterns to match.
This means that the use of ,@ or ,. is restricted to the end of a list
when used as a pattern.

FARE-QUASIQUOTE was originally written to work with FARE-MATCHER,
and legacy support for FARE-MATCHER is available in FARE-QUASIQUOTE-MATCHER,
now distributed with FARE-MATCHER itself. But FARE-MATCHER is deprecated.
OPTIMA supersedes FARE-MATCHER, and
FARE-QUASIQUOTE-OPTIMA supersedes FARE-QUASIQUOTE-MATCHER.


References
----------

Essential documents consulted while implementing this file:

  * [Alan Bawden's paper at PEPM 99](http://www.bawden.org/ftp/pepm99.ps.gz)
  * [CLtL2](http://www.supelec.fr/docs/cltl/clm/node367.html)
  * [CLHS](http://www.lisp.org/HyperSpec/Body/sec_2-4-6.html)
  * [Slate reference manual section 2.6.2 on quoting and unquoting](http://slate.tunes.org/doc/progman/node12.html#SECTION00046200000000000000)
  * Common Lisp backquote implementation, written in Common Lisp. (public domain)
     Author: Guy L. Steele Jr.     Date: 27 December 1985.
     To be used with patch by Alex Plotnick 2010 regarding the simplification pass.
  * SBCL backquote implementation (derived from CMUCL, used the October 2010 version).


Read-time vs Macro-expansion-time
---------------------------------

In conformance with the CLHS, FARE-QUASIQUOTE expands its patterns at read-time,
at least unless you enable a feature #+quasiquote-at-macro-expansion-time.

In both cases, FARE-QUASIQUOTE handles unquoting in #() and #n() syntax
by re-defining the syntax for hash-left-paren as well as for backquote.

If you enable feature #+quasiquote-at-macro-expansion-time,
FARE-QUASIQUOTE will expands its patterns at macro-expansion time,
using the same convention as Scheme, with symbols
quasiquote, unquote, unquote-splicing and unquote-nsplicing
defined in package FARE-QUASIQUOTE (but not exported from it).


Known Issues
------------

* Either at read-time or at macro-expansion-time,
 the implementation of quasiquote is fragile
 in case the user explicitly uses its internal syntax marker
 inside the expression being quasiquoted.
 Such expressions may lead to confusion between the body of expressions
 being quasiquoted and the internal quasiquote infrastructure.
 If you use these kinds of tricks, you're on your own.
 In fare-quasiquote, the syntax markers are the
 quasiquote, unquote, unquote-splicing and unquote-nsplicing symbols
 present in the fare-quasiquote package and not exported.
 Any objects that would be used as markers instead of these interned symbols
 would lead to the same "bug" if somehow used inside quasiquoted expressions;
 at least, non-interned symbols or gensyms would allow to avoid this bug
 in expressions being READ. The "perfect" solution would be to use
 objects GENSYMed at the moment a given QUASIQUOTE is read,
 and kept in a lexical variable to prevent introspective cheat by #. syntax.
 Then, after simplifying expressions, we could pass the read expression
 through functions that would SUBSTitute proper symbols for the markers,
 from the quasiquote package if pretty-printing is to be supported,
 or otherwise from the CL package.
 Note that this would have to be interleaved with support for working
 inside vectors and other syntax. This is all very tricky
 and until further notice is left as an exercise to the intrepid reader.
 Thus, while the behaviour of our implementation isn't strictly correct,
 we don't go through the hassle of modifying it into something
 much less readable just for the sake of preventing code that would
 deliberately confuse the quasiquote engine.
 Now, if we imagine that some code were dynamically generated
 based on system introspection, that could contain some of our magic markers,
 then this code would have to be made safe for interaction with QUASIQUOTE;
 this might (or might not?) require making QUASIQUOTE 100% fool-proof.

* This implementation allows for simplifying quasiquoting of literals
 and other self-evaluating objects into the object itself,
 instead of a `(QUOTE ,object) expression,
 if you enable the #+quasiquote-passes-literals feature.
 This is the behaviour of the simplifier in CMUCL and SBCL,
 but some people have expressed concerns that it might not be
 strictly allowed in letter or spirit by the Common Lisp standards,
 and it can make the pretty-printer trickier.

* This version works inside simple vectors, so as to support
 unquoting inside the #(...) syntax as the standard mandates.
 To do that, it replaces the hash-left-paren reader-macro
 as well as the backquote reader-macro.
 Note that this does not work in #1A(...) syntax.
 This phenomenon has been documented before in the following message:
	http://groups.google.com/groups?q=author:kaz%40ashi.footprints.net&hl=en&lr=&ie=UTF-8&oe=UTF-8&safe=off&selm=cf333042.0303141409.bbf02e9%40posting.google.com&rnum=4

* Interestingly, I haven't seen the following problem stated, to know which is
 correct of `#5(1 ,@'(2 3)). In other words, does the read argument to #()
 mean something at read-time or at vector-creation-time?
 Of course, in the original intended usage, outside of any quasiquoting,
 read-time and vector-creation-time are one and the same.
 But what when quasiquote breaks up that identity?
 Our answer is that it means something at vector-creation-time.

* The CLHS section 2.4.6 says that `(x1 x2 ... xn . atom) is same as
 (append [ x1] [ x2] [ x3] ... [ xn] (quote atom)) -- mind that the atom is preserved.
 This means that you can't conformantly simplify `(a b ,@c) to `(a b . ,c)
 unless you know that c is a proper list (if it isn't, that's an error).
 Yet, the CLHS itself suggests the simplification, and all implementations tested
 agree that a final (quote nil) can be elided:

	for l in sbcl ccl clisp cmucl ecl abcl \
                 scl allegro lispworks gcl xcl ; do
	  cl -l $l -i \
	 '(format t "'$l': ~S~%" `(,@`(a b) ,@`c)))' \
	  2>&1 | grep "^$l:" # LW, GCL are verbose
	done

 yet at the same time, SBCL still complains about `(,@1).
 fare-quasiquote follows the consensus, unless #+quasiquote-strict-append is enabled.

 * The current implementation of fare-quasiquote tends to simplify away things
   like `,c to c or without #+quasiquote-strict-append also `(,@c) to c.
   These simplifications probably need to be somehow prevented by default.
   Maybe with various kinds of (identity) wrappers to indicate quoting-unquoting?


Meta Unquote Protocol (not implemented)
---------------------------------------

The CLHS specifies that quasiquoting works with simple-vector,
but not with other arrays (multi-dimensional or not simple),
and not with arbitrary structures.
However, a fully general quasiquote facility
would support quasiquoting within arbitrary syntax,
using a MUP: Meta Unquote Protocol.

The MUP would allow to extend the quasiquote mechanism with support
for new constant-building syntactic constructs as such constructs are defined.
Maybe we will end up with a full-fledge declarative infrastructure
for a Parser-Preprocessor-Pretty-Printer, like camlp4 only more declarative.

The MUP would have an abstract API for arbitrary readers;
existing syntax for vectors would implemented using the MUP.
For compliance reasons, further MUP extensions would be disabled by default,
but could be made available with a suitable function call,
e.g. for #A #S #P etc.

The MUP might also implement tagged (and multiple-valued?) quasiquotes, unquotes and quotes.

Note that copying and modifying read-tables is expensive,
that dynamically modifying and restoring read-tables might interfere with #. syntax,
and that caching modified read-tables will interfere with any subsequent modification
of a cached read-table, comparison not being possible.
This means that if we wanted the MUP to adapt to existing extensions
without modifying existing code, we would have to intercept the definition
of syntax reading functions before they are fed to either SET-MACRO-CHARACTER
or SET-DISPATCH-MACRO-CHARACTER, or intercept the entire reader protocol. Spooky.
Now, this also requires that the current depth of quasiquoting be consulted
any time any of the MUP-enabled constructors is read.

The principle of the MUP is that:

 * structure readers that don't want to support unquote MUST be wrapped into
  something that dynamically binds *quasiquote-level* to 0.

 * structure readers #C(ARGSYNTAX) that do want to support unquote
  MUST accumulate formal arguments to a structure constructor
  into a list ARGUMENTS, then, if *quasiquote-level* is 0, behave like
    #.(apply `#CONSTRUCTOR `#ARGUMENTS)
  otherwise, behave like
    ,(apply `#CONSTRUCTOR `#ARGUMENTS)
  where #CONSTRUCTOR is the name of the constructor for given structure,
  and #ARGUMENTS is whichever arguments have been deduced from the syntax,
  which may include as many levels of unquotations as *quasiquote-level* says.
  Note that in a strong sense, "#." is like "," assuming an infinite tower of
  read-time evaluators a la 3-LISP.

Note that the above is obscured because we're trying to discuss
the behaviour of quasiquote-containing programs without having
a meta-level quasiquoting facility that could distinguish
between what is constant or variable at the meta-level independently from
what is constant or variable at the base level: #CONSTRUCTOR and #ARGUMENTS
would be better specified through a special meta-level unquotation,
the above expressions being in a corresponding special quasiquotation.
A feature that would allow for clear separation of levels of meta-language
would be a tagged quasiquote feature, as in [Slate](http://slate-language.org/).

The idea of making circular data-structures work within quasiquotation
makes my head ache with overarching pain.
You're crazier than I am if you do it and do it right.

PS: If you're able to follow this discussion, you impress me.
Come join the TUNES project!

