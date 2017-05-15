# Cogent git mirror #

Cogent is a **co**llaborative **gen**eric **t**rips CVS module. The project is funded by DARPA under the Communication with Computers (CwC) program.

This git repo is a mirror of the TRIPS `cogent` CVS module.

Note that the `src/config/lisp/defsystem/defsystem-3.6i/` directory contains a modified, non-standard, non-official version of [MK:DEFSYSTEM](http://www.cliki.net/mk-defsystem) 3.6i. See the comments near the top of `defsystem.lisp` in that directory for its copyright notice and license.

The rest of the repository is licensed using the [GPL 2+](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) (see `gpl-2.0.txt`):

TRIPS Cogent system  
Copyright (C) 2016  Institute for Human & Machine Cognition

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

## Download ##

To get the source code, run this command:

```bash
git clone https://github.com/wdebeaum/cogent.git
```

This will create a `cogent/` directory under your current directory. This is often called `$TRIPS_BASE`.

## Prerequisites ##

You will also need a Lisp implementation (we tend to use SBCL 1.2+) as well as Java (1.7+), Perl, and some other relatively standard development tools. If you're on a Mac, I think XCode gets you most of this.

If you use SBCL, you need to make sure it has multithreading support (the pre-built binaries don't). On the Mac, an easy way to do this is through [MacPorts](http://www.macports.org/), using this command:

```bash
sudo port install sbcl +threads
```

You'll also need to get these files from [WordNet](http://wordnet.princeton.edu/):

 - http://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.bz2
 - http://wordnetcode.princeton.edu/glosstag-files/WordNet-3.0-glosstag.tar.bz2

and unpack them in the same directory, `/usr/local/share/wordnet/` (create the directory if necessary). You should end up with these two directories:

    /usr/local/share/wordnet/WordNet-3.0/
    /usr/local/share/wordnet/WordNet-3.0/glosstag/

Finally you will need [Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/) 3.5.2+. The latest version as of this writing is [3.7.0](http://nlp.stanford.edu/software/stanford-corenlp-full-2016-10-31.zip). Download that zip file and unpack it in `/usr/local/share/stanford-corenlp/` (again, create the directory if necessary). Unpacking the zip file should create the directory `/usr/local/share/stanford-corenlp/stanford-corenlp-full-2016-10-31/`.

## Configuring, making, installing ##

Once you have the prerequisites, run these commands to configure, build, and install TRIPS:

```bash
cd $TRIPS_BASE/src/
./configure --with-lisp=sbcl
make
make install
```

The `--with-lisp` option lets you tell `configure` the command to use to run lisp. By default it uses `lisp`, but SBCL doesn't call itself that. There are many other options to `configure`, which you can see by giving it the `--help` option. But for the most part the defaults should be fine. (Doing `./configure --help` in `src/` doesn't show you absolutely everything, though; you can see more if you go into one of the `src/config/*/` directories first. For example, in `src/config/lisp/`, it describes `--with-lisp` and a few others.)

`make install` will install TRIPS into `$TRIPS_BASE/etc/` and `$TRIPS_BASE/bin/`. The latter is for executable programs, the former for everything else.

## Running ##

To run the whole system at once, run this command:

```bash
$TRIPS_BASE/bin/trips-cogent -showgen
```

The system display includes a Chat window where the user can type in utterances. Since the system does not include a surface generation component, the system doesn't produce natural language in respose. Using the `-showgen` option makes the Chat window diaplay the interaction acts sent for generation; in most cases the content of these acts is fairly easy to decode.

If you want to see the graphical representation of the Logical Form (LF) resulting from parsing the user's utterances, first make sure you have Graphviz installed (see [these installation instructions](http://trips.ihmc.us/trac/drum/wiki/GraphvizInstallation)). Then, run the system using the `-graphviz-display` option set to `true`, e.g.:

```bash
$TRIPS_BASE/bin/trips-cogent -showgen -graphviz-display t
```

Alternatively, if you only want to see the LF output, you can use the [Cogent web service](http://trips.ihmc.us/parser/cgi/cogent). This service is also useful for testing [ontology mappings](http://trips.ihmc.us/ontology-mapper/ontology-mapper.html).

If you want access to the Lisp REPL, you need to run the Lisp and non-Lisp parts separately. First run the non-Lisp parts, e.g.:

```bash
$TRIPS_BASE/bin/trips-cogent -nolisp -showgen
```

Then in another window run the Lisp parts:

```bash
cd $TRIPS_BASE/src/Systems/cogent/
sbcl --load test --eval '(run)'
```

## Customization ##

The Cogent system includes components for language understanding and interpretation, the **Collaborative Problem Solving Agent** (CPSA), as well as other components that are typically part of a TRIPS-based dialogue system (e.g., a Chat component). However, it doesn't include a **Behavioral Agent** (BA) -- the component responsible for domain-specific problem solving. This component is crucial, in that it is the one that encodes the domain behaviors, thereby enabling the system to DO something. [This Google document](https://docs.google.com/document/d/1pz5QT2VW4YPyY7VsP1kibhlUTUZE8f9ZpQXEvDxGrg0/edit) describes the messages that the BA needs to subscribe to and the replies it can send back to the CPSA.

Cogent also lacks a **Generation** component (hence the suggestion above to use the `-showgen` option, so that a user can at least see some internal representation of what the system would "say"). Minimally, this component only needs to subscribe to and act on `GENERATE` requests (also described in the Google document referenced above). This component is, of course, needed for the system to be able to communicate back to the user. Typically, this communication is carried out using natural language. However, this is not a requirement -- it is entirely possible that the system could communicate using a different modality, or use a mix of modalities.

For guidance on how to create new TRIPS components, look inside the [src/Hello](src/Hello) folder, which includes examples in several popular programming languages.

Depending on the domain, one may also need to add lexicon, domain-specific ontology, ontology mappings, etc.; these kinds of customizations are beyond the scope of this README file.
