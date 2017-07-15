(in-package :cm)

#|
In a Markov process past events represent a state, or context, for
determining the probabilities of subsequent events.  The number of
past events considered by the process is called its order. In a first
order process the probabilities for choosing the next event depend
only on the immediately preceding event. In a second order Markov
process the probabilities for the next choice depend on the last two
events.  A Markov process can reflect any number of past choices,
including the degenerate case of no past choices. A zero order Markov
Process is equivalent to weighted random selection.

Because past choices influence subsequent choices, a Markov process
can model, or mimic, different degrees of variation in patterns of
data. The higher the order, the closer the process comes to matching
the pattern it models. The pattern on which a Markov process is based
can determined by a statistical analysis of data or determined
directly by the composer.  We will examine both methods for creating
Markov processes later in this chapter.

Markov Statistical Analysis

By analyzing data and extracting a set of outcomes and weights, one
can create Markov processes that generate chains with statistically
similar characteristics. An analysis for a first order table considers
successive pairs in the sequence: the first element in the pair
represents a past outcome and the second represents its successor. In
a second order analysis the window is 3 elements wide, the first two
elements are the past outcomes and the third element is the
successor. The analysis proceeds by moving the window over successive
elements in the data and counting the number of times each unique
outcome follows each unique past. We can perform an example first
order analysis on the following short data sequence:

a a b c b a

Which produces the following analysis pairs

aa, ab, bc, cb, ba, aa

The last pair in the analysis (aa) is produced by wrapping around to
the front of the sequence again so that the last element in the
sequence also has a successor. An analysis that wraps around creates a
seamless chain without any terminating condition. (An alternate method
of analysis encodes a unique terminal value, call it z, as the
consequent of the last a. When z appears from the chain the caller
will knows that the Markov process cannot generate any more events.)

The next step in the analysis is to count each unique outcome for each
unique past, giving us the raw counts shown here

        a       b       c
a	2	1       -
b	1	-	1
c	-	1	-

and the following normalized probabilities

        a       b       c
a	0.667	0.333	-
b	0.500	-	0.500
c	-	1.000	-

It should be obvious from even this short example that Markov analysis
by hand is both tedious and error prone. Luckily for us, its iterative
nature makes it a chore that computers can easily do!

So lets try it with this sample melody and playback function.
|#

(defparameter name-that-tune
  (note '(c4 c d c f e c c d c g f 
          c c c5 a4 f e d bf bf a f g f)))

(defun play-tune (tune order reps rate)
  ;; process to play a markov chain created from tune
  (let ((pat (markov-analyze tune :order order 
                             :print? false)))
    (process repeat reps
             output (new midi :time (now)
                         :keynum (next pat)
                         :duration (* rate 1.5))
             wait rate)))

#|
We first perform a 1st order analysis of our unknown tune using the
markov-analyze function, which will print the results of the analysis
and create a markov pattern that, if executed, will generate items
according to the transition table shown in the analysis output.
|#

(markov-analyze name-that-tune :order 0)

#|
Each "row" in the pattern represents a "transition" where the left hand
side (values to the left of the transition marker ->) represents a
past choice and the right hand side contains the possible outcomes,
where each outcome is a list (x p) where x is the outcome and p is its
probability.

Now generate a file that contains the markov data from the pattern and
listen to it.
|#

(events (play-tune name-that-tune 1 40 .15) "markov.mid" 
        :versioning true)

#|
Can you name that tune? Maybe not, since first order does not generate
melodic fragments that are long enough to recognize. Lets try second
order:
|#

(markov-analyze name-that-tune :order 2)

(events (play-tune name-that-tune 2 40 .15) "markov.mid")
(events (play-tune name-that-tune 3 80 .15) "markov.mid")

#|
Note that in a second order analysis outcomes require two past choices
in the left hand side of each transition. This means that the outcomes
are more closely matching the actual tune so the process will generate
more recognizable melodic fragments.

Continue using markov analyze until you recognize the tune!


Stephen Foster Example

The book Computer Music (Charles Dodge) contains a very nice example
of a second order Markov chain based on an analysis of the complete
works of the American folk composer, Stephen Foster. Load the file
"foster.cm" located in CM's /etc/examples directory:
|#

; (load "/path/to/cm/examples/foster.cm")

#|
Once foster.cm has been loaded the following call will generate 4
parallel foster processes in different octaves. Select "Slow Strings"
on the four channels to get a pretty good Copeland sound....
|#

(events (list (new midi-program-change :time 0 
                   :channel 0
                   :program +orchestral-strings+)
              (foster 80 -12 0)
              (foster 80 0 0)
              (foster 80 12 0)
              (foster 80 24 0))
        "foster.mid")

#|
Try listening to different numbers of voices with different
sounds. Here are some other sound to choose from:
|#

+orchestral-strings+
+string-ensemble-1+
+string-ensemble-2+
+synthstrings-1+
+synthstrings-2+
+choir-aahs+
+voice-oohs+
+synth-voice+

#|
Markov Designs

A markov pattern does not have to be derived from existing data,
composers can work directly from their imagination. Consider the
problem of generating a melody for a soloist in which rhythmic values
are determined by uniform random selection. Even if only very simple
rhythms are used, a moments reflection will tell us that the patterns
produced by the process will be very difficult to play because the
uniform distribution will not reflect any underlying pattern of
beat. For example, assume that the process is restricted to quarters,
dotted-eights, eighths, and sixteenths (which we notate q, e., e, and
s respectively). Since the random process can place sixteenths and
dotted eighths anywhere within a beat, the sequence of rhythmic values
that result will only occasionally line up with the start of a metric
pulse.

But using a Markov pattern we can generate random rhythms that also
express an underlying beat that can be easily heard even in a tempo
curve.
|#

(defparameter tcurve '(0 1 .7 .75 1 1))  ; tempo envelope

(defun markov-rhythms (len tmpo)
  ;; rhythms generated from first order Markov
  (let ((rhys (new markov
                :of '((q -> (q .5) (e  2) (e. .75) )
                      (e -> (e 3) (q 1) )
                      (e. -> (s 1))
                      (s  -> (e 2) (q 1))))))
    (process for i below len
             for k = 60 then (drunk k 6 :low 40 :high 80 
                                    :mode ':jump)
             for r = (next rhys)
             for d = (* (rhythm r tmpo)
                        (interpl (/ i len) tcurve))
             output (new midi :time (now) :keynum k
                         :duration d)
             wait d)))

(events (list (markov-rhythms 60 120)
               (markov-rhythms 60 120))
        "markov.mid"
        )

#|
The Markov pattern assigns highest weight to eighth notes and the
least weight to dotted eighths. If a dotted-eighth is played then the
next value must be a sixteenth, which forces the pattern to return
back to the beginning of the beat again. A random walk generates the
melody.

Markov melody

In this next example we use a Markov process to generate a pseudo
Gregorian Chant. The intention is not really to generate a
stylistically correct chant, but rather to see how melodic motion can
be constrained using a Markov chain and how the processes can mimic
composition styles.

In the crudest of terms, a Gregorian Chant is characterized by mostly
step wise motion within a range of modal degrees. From any given tone
there is more likelihood that a chant will move a step up or down than
leap to a tone further away. The larger the skip, the more unlikely it
is to occur. In addition, certain tones, such as the final and tenor,
have more influence over the melody than other tones. The final of the
mode acts as a kind of reflecting boundary that redirects the melody
in the opposite direction. In the Dorian node (white notes starting on
D) the tenor note A is occasionally decorated by the B-flat one
half-step above above it. If the B-flat does occur, it always returns
back to the tenor tone. In an authentic mode. We can mimic these
stylistic tendencies using a first order Markov process.
|#

(defun chant-dur (tone dur)
  ;; adjust dur if tone is D, F or A.
  (let ((doub (* dur 2)))
    (if (scale= tone 'd4)
      (odds .7 doub dur)
      (if (scale= tone 'a4)
        (odds .5 doub dur)
        (if (scale= tone 'f4)
          (odds .25 doub dur)
          dur)))))


(defun monks (end rhy amp &optional (oct 0))
  (let ((chant
         (new markov
           :of '((d4 ->  (d4 .1) (e4 .35) (f4 .25) (g4 .1) (a4 .15))
                 (e4 -> (d4 .35) (f4 .35) (e4 .1) (g4 .1) (a4 .1))
                 (f4 -> (d4 .2) (e4 .2) (f4 .1) (g4 .2) (a4 .12))
                 (g4 -> (d4 .2) (e4 .1) (f4 .3) (g4 .1) (a4 .3) (bf4 .2))
                 (a4 -> (d4 .1) (e4 .2) (f4 .25) (g4 .3) (a4 .1) (bf4 .3))
                 (bf4 -> (a4 1))))))

    (process for k = (next chant)
             for dur = (chant-dur k rhy)
             output (new midi :time (now) 
                         :keynum (transpose k oct)
                         :amplitude amp
                         :duration dur)
             wait dur
             until (and (> (now) end)
                        (scale= k 'd4)))))

(events (list (new midi-program-change :time 0
                   :program +choir-aahs+ :channel 0)
              (monks 20 .8 .2))
        "markov.mid")

#|
Here are some other voice patches to try.
|#

+choir-aahs+
+voice-oohs+
+synth-voice+

#|
The Gregorian Chant is represented by a markov pattern prefers step
wise motion. Certain leaps, for example to the B-flat, are not allowed
at all. B-flat can only be approached and left from the tenor note
A. D4 is the final of the mode and acts as a reflecting boundary that
reflects the melody upward, usually by step, but occasionally to the
mediant or tenor tones as well.

The process generates notes from a first order Markov chant and uses
chant-dur to determine the rhythmic value for that tone. chant-dur
tests each note to see if it is the final D, tenor A or mediant
F of the mode. If the note is the final then there is a 70% chance
that its duration will be doubled. If k is the tenor then there is a
50% chance the tone will be doubled and if it is the mediant f4 then
there is a 25% chance it will be doubled. The until clause stops the
process if the current note is the mode's final and enough notes have
been sung. The oct parameter allows us to transpose the monk process
up and down without having to alter the process definition. This
allows us to listen to monks in octaves and other interval relations.
|#

(events (list (new midi-program-change :time 0
                   :program +choir-aahs+ :channel 0)
              (monks 30 .8 .2 0)
              (monks 28 .8 .2 -12)
              (monks 26 .8 .2 0)
              (monks 24 .8 .2 12))
            "markov.mid"
            '(0 2 4 8))

(events (list (new midi-program-change :time 0
                   :program +choir-aahs+ :channel 0)
              (monks 24 .8 .2 -6)
              (monks 24 .8 .2 0)
              (monks 24 .8 .2 6))
            "markov.mid"
            '(0 0 3 6))




