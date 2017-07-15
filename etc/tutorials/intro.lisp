(in-package :cm)

#|

Common Music represents musical structure using "objects".  An object
is an entity containing properties, or slots, whose values can be
assigned by the user or a program the user designs. We start out this
tutorial by learning about three important types, or classes, of
objects: Event, Seq and Process.

Events

An event is an object that represents some sort of sound.  When CM
starts up it already contains a number of commonly used event classes
and you can define new ones as you need them.  In this tutorial we
will work with an object called a MIDI event. To create a MIDI event
you use the new macro:
|#

(new midi)
(new midi :time 1)
(new midi :keynum 66)
(new midi :time 0 :keynum 'c4 :duration 2 :amplitude .2 :channel 0)

#| 
The syntax of new is simple, its first argument is the unquoted name
of the class (type) of object to create and the rest of the arguments
are "slot initialization pairs", where each pair consists of a keyword
slot name followed by its value. A keyword is a Lisp symbol that
starts with :, for example:
|#

:keyword
:another-keyword

#|
So the Lisp expression:
|#

(new midi :time 1 :amplitude .1)

#|

creates a new MIDI object whose :time is 1 and amplitude is .1.  With
the notable exception of the time slot, most slots have "default
values" that are used if the composer does not specify a value when
the object is created. For MIDI objects the default :keynum value is
60 (Middle C), :duration is .5 (half a second), :amplitude is .5 (medium
loud) and :channel is 0 (channel 0 is channel 1 in zero-based
counting). See the Common Music Dictionary for more information about
MIDI objects.

Seq

A seq is an object that contains a time ordered sequence (list) of
subobjects. Let's create one using a loop to collect 80 midi objects
for it to hold:
|#

(new seq :name 'pulse
     :subobjects
     (loop with d = .1 
           for i below 80
           collect (new midi :time (* i d)
                        :keynum (pickl '(c4 d4 ef4 f4 g4 af4 bf4 c5))
                        :duration d
                        :amplitude (interp (mod i 8) 0 .25 7 .75))))

#| 
Seqs (and other types of containers) can have names; a named object
can be fetched from its name using find-object or the #& read macro
|#

(defparameter p (find-object 'pulse))

(eq p #&pulse)

#|
To view the contents of a seq you can use subobjects or the
list-objects function. Let's list just the first 10 objects:
|#

(list-objects #&pulse :end 10)

#|
It is sometimes useful to list, collect or edit non-adjacent elements:
|#

(list-objects #&pulse :start 10 :end 20 :step 2)

(list-objects #&pulse :at '(0 45 70))

(subobjects #&pulse :at '(0 45 70))

#|
Processes

A process is a function that runs, or executes, inside a scheduler to
accomplish some musical task. Its always a good idea to define a
process inside a function because then different variations of the
process can be instantiated simply by calling the named function with
different input values. Lets try it:
|#

(defun sinus (len cycl low hi rhy dur amp)
  (process for i below len
           output
           (new midi :time (now)
                :keynum (rescale (sin (* 2 pi (/ i len) cycl))
                                 -1
                                 1
                                 low
                                 hi)
                :amplitude amp 
                :duration dur)
           wait rhy))

#|

Sinus is a function that creates a process. The process will play midi
notes in a "sine wave" of cycl periods over len number of notes. sin's
value (-1 to 1) is rescaled to lie between the specified low and hi
keynums. Each time the process outputs a midi event it waits for a
specified amount of time before running again. Any number of processes
and seq can run inside the scheduler; its the scheduler job to make
sure that all events happen at the correct time in the score.  Lets
see what happens when we call the sinus function to create processes:
 |#

(sinus 80 4 20 100 .1 .1 .6)  

(sinus 60 5.7 50 80 .1 .1 .6)

#|
Notice that you didn't actually hear anything...This is because the
function only creates a process, it doesn't execute it.  In order to
generate output from a process you need to pass it to the event
scheduling function along with some output destination. Most (but not
all) destinations are simply files on the disk. You can specify full
pathnames for files or you can set CM's working directory and then
provide just the file name and extension without having to type the
full directory path. You use pwd and cd to do this:
|#

(defparameter here (pwd))    ; save the current working directory

(cd)                 ; go to your home directory

(cd "/tmp")          ; the / will work on Linux, OS X and Windows

(cd here)            ; go back where you were

#|
Assuming you did a (cd) to some directory where you have write
permission, you can now use events to generate the sinus process to a
midi file in the current working directory. So let's generate a midi
file and play it!
|#

(events (sinus 80 4 20 100 .1 .1 .6) "intro.mid" :versioning true)

#| 
All files allow :versioning and :play inits to be specified. These
setting are "sticky", i.e. their current value remains in effect until
you explicitly change it. If :versioning is true then numbers are
appended to the output file name so that new output does not overwrite
earlier versions of the file. The default value of :versioning is
false. The :play init determines if the file will be played
automatically as soon as its written.  Each type of output file (.mid,
.osc, .clm, .sco, and so on) has a player associated with it that is
called after a file of that type is written. Before attempting to
generate a midi file first check the value of *midi-player* to see if
it is valid for your machine. If it is not then set it to some
appropriate command string:
|#

*midi-player*

#|
If you want to listen to the file again without generating it anew,
use the play function directly.
|#

(play "intro-1.mid")

#|
If you want to generate a file but don't want to hear it immediately,
pass :play false to events.
|#

(events (sinus 100 6 90 50 .1 .2 .6) "intro.mid" :play false)

(play "intro-2.mid")

#|
Now turn auto playback on as we generate pulse.
|#

(events #&pulse "intro.mid" :play true)

#|
Setting *midi-player* and *audio-player* is a fine thing to do in your
~/.cminit.lisp file.

Now lets use events to listen to sinus and pulse together, starting
two seconds apart in the output file.
|#

(events (list (sinus 80 4 20 100 .1 .1 .5)
              #&pulse)
        "intro.mid"
        '(0 2))

#|
You can mix any number of objects together by specifying them as a
list to the events function. If offsets are specified, each object in
the list will be matched in left to right order with its corresponding
start time in the offset list.

TRY:

1. Generating different versions of sinus
2. Mixing sinuses and pulses together at different offsets
3. Defining new pulses
4. Defining variants of sinus that use Lisp functions other than sin.
5. Defining variants of sinus that also compute values for rhythm,
   duration and amplitude.
|#

