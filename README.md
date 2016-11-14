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

And unpack them in the same directory, `/usr/local/share/wordnet/` (create the directory if necessary). You should end up with these two directories:

    /usr/local/share/wordnet/WordNet-3.0/
    /usr/local/share/wordnet/WordNet-3.0/glosstag/

Finally you will need [Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/)  3.5.2+. The latest version as of this writing is [3.7.0](http://nlp.stanford.edu/software/stanford-corenlp-full-2016-10-31.zip). Download that zip file and unpack it in `/usr/local/share/stanford-corenlp/` (again, create the directory if necessary). Unpacking the zip file should create the directory `/usr/local/share/stanford-corenlp/stanford-corenlp-full-2016-10-31/`.

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
$TRIPS_BASE/bin/trips-cogent
```

If you want access to the Lisp REPL, you need to run the Lisp and non-lisp parts separately instead. First run the non-Lisp parts:

```bash
$TRIPS_BASE/bin/trips-cogent -nolisp
```

Then in another window run the Lisp parts:

```bash
cd $TRIPS_BASE/src/Systems/cogent/
sbcl --load test --eval '(run)'
```

