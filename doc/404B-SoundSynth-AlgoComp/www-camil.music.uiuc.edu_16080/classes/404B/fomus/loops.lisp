;; LOOPS
					;
iterating w/ "for"--"for" goes at beginning of loop
(loop for i from 1 to 10 do (print i)) ; "do" executes instructions
(loop for i from 1 to 10 do (print i) (print (+ i 100)))
(loop for i from 1 to 10 collect i)
(loop for i from 10 downto 1 collect i)
(loop for i from 0 below 10 collect i)
(loop for i from 1 to 5 and j from 1 to 10 collect (list i j))

; other ways to use "for"
(loop for i from 1 to 5 for j = i collect j)
(loop for i from 1 to 5 and j = i collect j) ; using "and" is a little different (i and j update in parallel, not sequentially)
(loop for i from 0 to 10 by 2 collect i)
(loop for i in '(1 2 3 4 5) collect i)
(loop for (i j) in '((1 a) (2 b) (3 c) (4 d)) collect (list j i)) ; (i j) has the same structure as '(1 a), '(2 b), etc.., so lisp binds i to the numbers and j to the symbols
(loop for (i (j k)) in '((1 (a aa)) (2 (b bb)) (3 (c cc))) collect (list k j i))
(loop for (i j) on '(1 2 3 4 5 6 7) collect (list i j)) ; "on" instead of "in"
(loop for i across #(1 2 3 4 5) collect i) ; use "across" for vectors or arrays
(loop for i = 1 then (* i 2) repeat 10 collect i) ; the "repeat" must be there to end the loop, otherwise it will run forever
(loop for i1 = 0 then i2 for i2 = 0 then i3 for i3 = 1 then (+ i2 i1) repeat 100 collect i3) ; first 100 fibinacci numbers

; other ways to iterate
(loop repeat 5 do (print "HI!"))

 ; append vs. collect
(loop repeat 5 append '(a b c)) ; you can also use "nconc" instead of "append", but "nconc" is destructive (use append to always be safe!)
(loop repeat 5 collect '(a b c))
(loop repeat 5 append (loop for i from 10 downto 1 collect i))

; more collecting things (not all of these are the most efficient way of doing things)
(loop for i from 1 to 10 for j = (* i i) collect i collect j) ; multiple collects & appends
(loop for i from 1 to 10 for j = (loop for j from 1 to i collect j) append j append j)
(loop for i from 1 to 10 for j = (loop for j from 1 to i collect j) collect i append j)
(loop for i from 1 to 10 collect (/ i) into x finally (return x)) ; code after "finally" runs after loop is finished, "return" returns the value

; while, until
(loop for i from 1 while (< i 20) collect i)
(loop for i from 1 collect (- (/ i)) into x collect (/ i) into y until (> i 10) finally (return (list x y))) ; while and until will trigger the end of the loop, but the "finally" clause still executes

; "with" variables--go at the beginning of the loop
(loop with x = (sqrt pi) for i from 1 to 10 collect (* i x))
(loop with x = '(1 0) repeat 100 do (push (+ (first x) (second x)) x) finally (return x)) ; backwards fibinnaci

; "when", "unless", "if", "else"--these work just like the lisp functions
(loop with x and y for i from -5 to 5 when (oddp i) do (push i x) when (plusp i) do (push i y) finally (return (list x y))) ; "with x and y"--"and" binds them in parallel--"with x with y" would bind them sequentially
(loop with x for i from -5 to 5 if (oddp i) do (push i x) else collect i into y finally (return (list x y)))
(loop for e in '(a b c d e f) unless (find e '(a c e)) collect e)



;; EXTREMELY USEFUL LISP FUNCTIONS
(progn ; evaluates expressions sequentially, returns the last evaluation
  (print "A")
  (print "B")
  5)

(let ((x 5)) ; progn is very useful in "if" constructions if you want "if" to branch to multiple lines of code
  (if (= x 5)
      (progn (print "YES") 1)
      (progn (print "NO") 2)))

(let ((x 5)) ; "when" doesn't need "progn"--this works exactly like (if (= x 5) (progn (print "YES, x equals 5") 1) nil)
  (when (= x 5)
    (print "YES, x equals 5")
    1))

(let ((x 5)) ; "unless" doesn't need "progn"--this works exactly like (if (= x 4) nil (progn (print "X doesn't equal 4") 2))
  (unless (= x 4)
    (print "X doesn't equal 4")
    2))

(let ((x 5)) (case x (1 "ONE") (2 "TWO") (3 "THREE") (otherwise "NOTHING")))
(let ((x 'c)) (case x (a "ONE") (b "TWO") (c "THREE") (otherwise "NOTHING")))
(let ((x 5)) (cond ((< x 3) "LESS THAN 3") ((> x 6) "GREATER THAN 6") (t "NOT ONE OF THE OTHER ONES"))) ; works like consecutive "if" functions
(let ((x nil) (y 6) (z 10)) (or x y z)) ; or returns the first expression that evaluates to t, so it's useful sometimes to select an "alternate" value

(let ((x 5)) (= x 5)) ; use "=" for numbers
(let ((x 'a)) (eql x 'a)) ; use "eql" for symbols and numbers
(let ((x '(a b (c d)))) (equal x '(a b (c d)))) ; use "equal" for lists (and nested lists)
(let ((x (list 3 (vector 'a 'b 'c)))) (equalp x (list 3 (vector 'a 'b 'c)))) ; use "equalp" when arrays/vectors are involved

(+ 1 2 3 4 5)
(apply #'+ '(1 2 3 4 5)) ; calls a function with the arguments in a list
(max 1 2 3 4 5)
(apply #'max 1 2 '(3 4 5))

; lists
(length '(a b c d e))
(subseq '(a b c d e) 2 4)
(reverse '(1 2 3 4 5))
(rest '(1 2 3 4 5))
(butlast '(1 2 3 4 5))
(butlast '(1 2 3 4 5) 3)
(last '(1 2 3 4 5))
(append '(1 2 3) '(4 5))
(cons 1 '(2 3 4 5))
(first '(1 2 3))
(second '(1 2 3))
(third '(1 2 3))
(nth 2 '(1 2 3))
(let ((x '(2 3 4)))
  (push 1 x)
  x)
(let ((x '(1 2 3 4)))
  (print (pop x))
  x)
(let ((x '(2 3 4)))
  (pushnew 1 x)
  (pushnew 2 x)
  x)
(find 'b '(a b c d e))
(position 'c '(a b c d e))

(intersection '(a b c d e) '(c d e f g))
(set-difference '(a b c d e) '(a b c))
(union '(a b c d e) '(c d e f g))
(remove-duplicates '(a b c c b d))
(sort '(2 4 6 8 1 3 5 7) #'<) ; destructuve!
(sort (copy-list '(2 4 6 8 1 3 5 7)) #'<) ; use copy-list for non-destructive
(remove 'c '(a b c d e))

;; vectors/arrays
(let ((x (vector 'a 'b 'c 'd 'e)))
  (aref x 1))
(let ((x (make-array 5 :initial-contents '(a b c d e))))
  (aref x 1))
(length (vector 'a 'b 'c 'd 'e))
(subseq (vector 'a 'b 'c 'd 'e) 2 4)
(sort (vector 5 4 3 2 1) #'<)
(find 3 (vector 5 4 3 2 1))
(position 3 (vector 5 4 3 2 1))