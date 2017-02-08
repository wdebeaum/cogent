Hello - examples of a simple TRIPS module/component in different languages
William de Beaumont
2017-02-08

This directory contains a very simple TRIPS module/component that replies to
requests of the form:

  (request :content (hello) :sender fred)

With messages of the form:

  (tell :content (hello fred) :receiver fred)

This module has been written several times in different programming languages,
in order to demonstrate how to write a TRIPS module in each of those languages.


Files by language:

All
	README.txt	This file.
	Makefile	Demonstrates how to make a module with pieces in
			different languages (by calling separate makefiles for
			those languages). Doesn't really work in this case
			because the different languages all want to make an
			executable file called "Hello".

C/C++
	[TODO; see $TRIPS_BASE/src/trlib/ and $TRIPS_BASE/src/Tools/]

Java
	Makefile-java	Build instructions for Hello.java.
	Hello.java	Main code.
	[most other languages follow this pattern; only Lisp is different]

JavaScript
	Makefile-javascript
	Hello.js

Lisp
	Makefile-lisp	Basically just points to defsys.lisp.
	defsys.lisp	Component definition (lists sources and dependencies).
	hello.lisp	Loading program.
	messages.lisp	Message handler (this is the main code for the
			component).

Perl
	Makefile-perl
	Hello.pl

Python
	Makefile-python
	Hello.py

Ruby
	Makefile-ruby
	Hello.rb

Scala
	Makefile-scala
	Hello.scala	Note that this uses Java's TripsModule implementation.


Using the Makefiles:

Use the -f option of make to select a specific language. For example, to clean, build, and install the Scala version of the Hello module, do this:

  make -f Makefile-scala clean
  make -f Makefile-scala
  make -f Makefile-scala install

It's also possible to clean everything:

  make clean

Or build a specific language's module like this:

  make scala

But to install, you need to use -f as above. "make install scala" doesn't do
what you might want it to. It first tries to install everything (which won't
work, because the different languages all try to write the same file, "Hello"),
and then tries to build the Scala-langauge module only.

For languages other than Scala and Lisp, it may also be necessary to
build and install the KQML and TripsModule libraries in the same
language-specific way in order to make Hello work. For Scala, use the Java
versions of those libraries.


Running the modules:

Most versions will connect to the Facilitator on port 6200 of the local machine when you run the Hello program with no arguments:

  ./Hello

And you can connect differently using the -connect argument:

  # connect to port 6205 on example.com:
  ./Hello -connect example.com:6205

  # pretend stdin/stdout is the connection to the Facilitator (this is useful
  # for testing your module by manually typing messages to it):
  ./Hello -connect no

Lisp is different. To connect to stdin/stdout (like "-connect no" above), you
evaluate these lisp commands:

  (load "hello")
  (run)

To connect to the local Facilitator:

  (load "hello")
  (setf io::*transport* :socket)
  (run)

If you're using sbcl, you can do this from the command line (other Lisp
implementations have similar, but not always the same, arguments):

  sbcl --load hello --eval '(setf io::*transport* :socket)' --eval '(run)'

