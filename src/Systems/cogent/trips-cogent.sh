#!/bin/sh
#
# File: trips-cogent.sh
# Creator: Lucian Galescu, based on template by George Ferguson
# Created: Wed Jun 20 10:38:13 2012
# Time-stamp: <Wed Nov 30 12:55:24 CST 2016 lgalescu>
#
# trips-cogent: Run TRIPS/COGENT
#
# This script uses the following environment variables, if set:
#  TRIPS_BASE			Root of TRIPS directory tree
#  TRIPS_LOGS			Where to save the logs
# Run with -help to see the usage message.
#

echo 'This is TRIPS/COGENT version 0'
TRIPS_BASE_DEFAULT=/usr/local/trips

# Set TRIPS_BASE unless set
if test ! -z "$TRIPS_BASE"; then
    echo "Using your TRIPS_BASE=\"$TRIPS_BASE\""
else
    TRIPS_BASE=$TRIPS_BASE_DEFAULT; export TRIPS_BASE
    echo "Using TRIPS_BASE=\"$TRIPS_BASE\""
fi

TRIPS_HOST_DEFAULT=localhost
TRIPS_PORT_DEFAULT=6200

TRIPS_SYSNAME=cogent
TRIPS_SYSNAME_ALLCAPS=`echo $TRIPS_SYSNAME | tr "[:lower:]" "[:upper:]"`

#############################################################################
#
# Command-line

usage="trips-$TRIPS_SYSNAME [-debug] [-port 6200] [-display tty] [-nouser] [-nochat] [-nolisp] [-nocsm] [-graphviz-display true]"

logdir=''
debug=false
port=''
nouser=''
nolisp=''
nocsm=''
who=User
channel=Desktop
display=''
graphviz_display=false
nochat=''
nobeep=''
showgen=false

while test ! -z "$1"; do
    case "$1" in
	-port)		port="$2";	shift;;
	-debug)		debug=t;;
	-nodebug)	debug='';;
	-who)		who="$2";	shift;;
	-channel)	channel="$2";	shift;;
	-display)	display="$2";	shift;;
	-logdir)	logdir="$2";	shift;;
	-nouser)	nouser=t;;
	-nolisp)	nolisp=t;;
	-nocsm)		nocsm=t;;
	-nochat)	nochat=t;;
	-nobeep)	nobeep=t;;
	-quiet)		nobeep=t;;
        -graphviz-display)      graphviz_display="$2";  shift;;
	-showgen)	showgen=t;;
	-help|-h|-\?)
	    echo "usage: $usage"
	    exit 0;;
	*)
	    echo "$0: unknown argument: $1" 1>&2
	    echo "usage: $usage" 1>&2;
	    exit 1;;
    esac
    shift
done

# beep?
if test \( -z "$nospeech" -a -z "$nospeechout" \) -o ! -z "$nobeep"; then
    beep_kbd=false
else
    beep_kbd=true
fi

# set port options
TRIPS_PORT=${port:-$TRIPS_PORT_DEFAULT}
TRIPS_SOCKET=${TRIPS_HOST_DEFAULT}:${TRIPS_PORT}
export TRIPS_SOCKET
port_opt="-connect $TRIPS_SOCKET"

# set default character encoding to UTF-8
export LC_ALL=en_US.UTF-8
export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8

# Make sure log directory exists
if test -z "$logdir"; then
    logdir=${TRIPS_LOGDIR:-${TRIPS_LOGS:-`pwd`}/`date '+%Y%m%dT%H%M'`}
fi
if test -d "$logdir"; then
    echo "Using log directory $logdir"
else
    echo "Creating log directory $logdir"
    mkdir -p "$logdir" || exit 1
fi
original_cwd=`pwd`
cd "$logdir" || exit 1

# Clean up any child process when we die
# Note [LG, 2011/03/11]: We used to use pkill to kill subprocesses. Turns out 
# this was not working well on Macs (for reasons i won't get into). 
# The solution below uses just ps and awk, which should be available on all the
# platforms we currently use. However, there are different implementations of 
# ps and awk out there; this code was only tested on my Mac (10.6.6).
trap cleanup 0 1 2 3 15

cleanup () {
    rm -f /tmp/trips$$
    rkill $$
    # will never get here
}

rkill() {
    for cpid in $(ps -o pid,ppid | awk -v ppid=$1 '$2==ppid {print $1}')
    do
	rkill $cpid
    done
    #echo "killing: $(ps -o pid=,command= $1 )"
    kill -9 $1 > /dev/null 2>&1
}

#############################################################################
#
# Here we go...
#

# The following will be sent to the facilitator (via stdin) once it starts
cat - <<_EOF_ >/tmp/trips$$
(register :name init)
(tell :content (status ready))
_EOF_
if test -z "$nouser" -a -z "$nochat" ; then
cat - <<_EOF_ >>/tmp/trips$$
(request
 :receiver facilitator
 :content (start-module
           :name keyboard
           :class TRIPS.KeyboardManager.KeyboardManager
           :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.KeyboardManager.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.util.jar")
           :argv (-geometry 60x6+0-0
                  -fontsize 16
                  -who $who
                  -channel $channel
                  -title "$TRIPS_SYSNAME_ALLCAPS: Chat"
                  -beep $beep_kbd
		  -showGenerate $showgen
		  $port_opt)))
_EOF_
fi

# SpeechOut
# We don't use speech-out at all (run SpeechOutNot to convert SAY to SPOKEN)
cat - <<_EOF_ >>/tmp/trips$$
(request
 :receiver facilitator
 :content (start-module
	   :name speech-out
           :class TRIPS.SpeechOutNot.SpeechOutNot
           :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.SpeechOutNot.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.util.jar")
	   :argv ($port_opt)
))
_EOF_

# CSM
if test -z "$nocsm"; then
cat - <<_EOF_ >>/tmp/trips$$
(request
    :receiver facilitator
    :content (start-module
    :name CSM
    :class TRIPS.CollaborativeStateManager.CollaborativeStateManager
    :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.CollaborativeStateManager.jar"
                "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
                "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
                "$TRIPS_BASE/etc/java/TRIPS.util.jar"
                "$TRIPS_BASE/src/CollaborativeStateManager/src")
    :argv ($port_opt
    	   -data "$TRIPS_BASE/etc/$TRIPS_SYSNAME")))
_EOF_
fi


# Modules not started by the Facilitator have to start *after* the
# Facilitator so they can connect, so we use the form (sleep 5; foo) &
# to start them

# Lisp
if test -z "$nolisp"; then
  (sleep 5; $TRIPS_BASE/bin/trips-$TRIPS_SYSNAME-lisp) 2>&1 | tee lisp.log &
fi

# Start TextTagger
(sleep 5; \
 $TRIPS_BASE/bin/TextTagger \
     $port_opt \
     -process-input-utterances yes \
     -terms-file $TRIPS_BASE/etc/$TRIPS_SYSNAME/domain-terms.tsv \
     -init-taggers terms-from-file \
     -default-type '(or affixes words punctuation terms-from-file)' \
 2>&1 | tee $logdir/TextTagger.err) &

# Start Graphviz
(sleep 5; \
 $TRIPS_BASE/bin/Graphviz $port_opt -display-enabled $graphviz_display \
 2>&1 | tee Graphviz.err) &

# set display option for facilitator           
if test -n "$nouser"; then
    display='none'
fi
if test -n "$display"; then
    display_opt="-display $display"
else
    display_opt=''
fi

# Launch facilitator and send initial messages via stdin
cat /tmp/trips$$ |\
 $TRIPS_BASE/bin/Facilitator -port $TRIPS_PORT -title $TRIPS_SYSNAME_ALLCAPS -geometry 260x600-0+0 $display_opt 2>&1 | tee facilitator.err &

# Wait for Facilitator to die
wait $!

# Bye
exit 0
