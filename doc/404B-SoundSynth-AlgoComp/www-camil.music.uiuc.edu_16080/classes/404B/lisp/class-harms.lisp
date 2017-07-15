(defun 16harms (fund)
  (loop for i from 1 to 16
       collect (* fund i)))

(16harms 100)
(16harms 123.45)

(defun harms (fund lb ub)
  (loop for i from lb to ub
       collect (* fund i)))

(defun k-harms (fund lb ub)
  (loop for i from lb to ub
       collect (keynum (* fund i) :hz t )))

(k-harms 100 1 16)

(defun k-harms2 (k-fund lb ub)
  (loop for i from lb to ub
       collect (keynum (* (hertz k-fund) i) :hz t )))

(defun k-harms2 (k-fund lb ub)
  (loop with x =  (hertz k-fund)
     for i from lb to ub
     collect (keynum (* x i) :hz t )))

(defun k-harms2 (k-fund lb ub)
  (let ((x (hertz k-fund)))
    (loop for i from lb to ub
       collect (keynum (* x i) :hz ))))

(k-harms2 48 1 16)

(cd)

(defun test-k-harms2 (kfund lb ub dur amp )
  (process for i in (k-harms2 kfund lb ub)
           output (new midi :time (now) :keynum i :duration dur
                    :amplitude amp )
           wait dur))

(events (test-k-harms2 40 7 14 .5 .8) "test.mid"
        :channel-tuning 2)


