(in-package :cm)

#|
In this tutorial we will learn how build up a short section of music
using some of the sequence editing functions in Common Music. We start
off by playing and then importing some MIDI source material from the
file "source.mid" located in the same directory as this file.
|#

(play "source.mid")

(defparameter s1 (import-events "source.mid" :meta-exclude true))

#|
The import-events function can be used to parse several different
types of score files (.midi, .clm, .osc and .sco) to create one (or
more) seq objects containing CLOS event objects. In our import-events
call we specified :meta-exclude to avoid bringing in MIDI meta-message
events in addition to MIDI note data.

As a start we find out the number of events in the file by taking the
length of the subobject list and then list the first five objects to
see what their data look like.
|#

(length (subobjects s1))

(list-objects s1 :end 5)

#|
To start off the editing session we will make a second seq that holds
an exact copy of s1's objects.
|#

(defparameter s2 (new seq :subobjects (mapcar #'copy-object
                                              (subobjects s1))))

#|
Next we will use map-object to transpose the newly copied material up
one octave (12 semi-tones) by mapping the keynum slot of each MIDI
object and setting it to whatever its current value is plus 12.
|#

(map-objects (lambda (k) (+ k 12)) s2 :slot! 'keynum)

#|
The :slot! keyword tells map-object to map and SET the specified slot
in each object it maps. Map-objects does not return any values. We can
now look at the transposed data and compare it to data we listed in
s1.
|#

(list-objects s2 :end 5)

#|
Note that the keynums in s2 are 12 higher than corresponding keynums
in s1. Now listen to both versions of the material, each offset by 5
seconds.
|#

(events (list s1 s2) "editing.mid" '(0 5) :versioning true)

#|
Next we edit s2 to give it a slightly more interesting effect by
boosting the overall amplitude and then additionally high-lighting
only G and A-flats. First, lets double all the amplitudes.
|#

(map-objects (lambda (a) (* a 2)) s2 :slot! 'amplitude)

#|
Now collect only G and A-flat notes into a hilite list to edit.
Since C is pitch class 0, G is 7 and A-flat (or G-sharp) is 8.
|#

(defparameter hilite (subobjects s2 :test (lambda (x)
                                            (member (mod x 12) '(7 8)))
                                 :slot 'keynum))

#|
To proves that we only collected G and A-flat notes lets use
fold-objects to collect the keynums in hilite and pass them to the
note function.
|#

(note (fold-objects (lambda (x l) (cons x l)) 
                    hilite
                    '()
                    :slot 'keynum))

#|
The fold-object function combines (folds) mapped entities. The
function you pass to it takes two args, a mapped object and the
combined value. the value returned by the function becomes the next
combined value.

By converting keynums to notes we can see that hilite holds the
subobjects we want to edit. We will now double the amplitude and the
duration of these notes. Rather than evaluating 2 mapping expression
(one for each slot) we can use the sv, sv+ and sv* macros to set,
increment and scale multiple slots of interest in one call.
|#

(map-objects (lambda (x) (sv* x :amplitude 2 :duration 2))
             hilite)

#|
Now listen to s1 and s2 together again.
|#

(events (list s1 s2) "editing.mid" '(0 5))

#|
Next we make a bass texture by copying s1 to a third seq and then
transposing this material down 2 octaves, or -24 semi-tones.
|#

(defparameter s3 (new seq :subobjects (mapcar #'copy-object
                                              (subobjects s1))))

#|
Since we transposed downward we will also boost these amplitudes
because lower tones require more energy to speak than higher tones
require.
|#

(map-objects (lambda (x)
               (sv+ x :keynum -24)
               (sv* x :amplitude 5))
             s3)

#|
Now lets emphasize just the lowest note in the bass texture. But how
can we set it if we don't know what the lowest note is?
|#

(fold-objects (lambda (k v) (min k v)) s3 128 :slot 'keynum)

#|
Fold objects returns the minimum keynum value found. The initial
combined value is 128 to ensure that it is higher than any MIDI keynum
in the data.

Now that we know what the lowest keynum is (36) we can map just these
notes by providing a predicate test to map-objects.
|#

(map-objects (lambda (x) (sv* x :duration 2 :amplitude 2))
             s3
             :test (lambda (x) (= (sv x :keynum) 36)))

#|

Now we will listen to all the material together with each voice offset
by five seconds.
|#

(events (list s1 s2 s3) "editing.mid" '(0 5 10))

#|
As a final step we will define a fore-ground texture to go with the
background. The fore-ground will consist of a process that plays
chords made up of random segments from from three C minor scales.
|#

(defun chorder (amp)
  (let ((pat1
         (new cycle 
           :of (list (new chord
                       :of (new heap :notes '(c6 d ef f g af bf)
                                :for 4))
                     (new chord
                       :of (new heap :notes '(c5 d ef f g af bf)
                                :for 4))
                     (new chord
                       :of (new heap :notes '(c4 d ef f g af bf)
                                :for 4))
                     'r)
           :repeat 6))
        (pat2
         (new cycle
           :of `(t ,(new cycle :of '(e q. h) :for 1)
                    e s.))))
    (process for n = (next pat1)
             for d = (rhythm (next pat2))
             until (eod? n)
;             do (print (list (now) n d))
             if (listp n)
             each k in n 
             output (new midi :time (now)
                         :amplitude amp
                         :duration d
                         :keynum k)
             wait d)))

(events (chorder .75) "editing.mid")

#|
The chorder process uses several patterns to generate its
data. Patterns are beyond the scope of this tutorial, you can read
about them in the Pattern topic help in the Dictionary.

Now we make a final mix of the three background processes together
with the chords.
|#

(events (list s1 s2 s3 (chorder .75)) "editing.mid" 
        '(0 5 10 15))




