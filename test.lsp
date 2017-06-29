;--------------------------------------------
;--------------------------------------------

(defmacro amin-run-time (&rest body)
  `(progn 
      (let (t1 t2))
      (setf t1 (get-internal-run-time))
      ,@body
      (setf t2 (get-internal-run-time))
      (- t2 t1)))

(defmacro amin-real-time (&rest body)
  `(progn 
      (let (t1 t2))
      (setf t1 (get-internal-real-time))
      ,@body
      (setf t2 (get-internal-real-time))
      (- t2 t1)))

;--------------------------------------------
;--------------------------------------------

(defmacro amin-run-test (n &rest body)
  `(progn 
    (let ((time 0))
    (loop for i from 0 to ,n
      do (setf time (+ time (amin-run-time ,@body)))
    )
    (float (/ time ,n)))))

(defmacro amin-real-test (n &rest body)
  `(progn 
    (let ((time 0))
    (loop for i from 0 to ,n
      do (setf time (+ time (amin-real-time ,@body)))
    )
    (float (/ time ,n)))))

;--------------------------------------------
;--------------------------------------------

(defun write-file (name content)
    (with-open-file (stream name
        :direction :output
        :if-exists :overwrite
        :if-does-not-exist :create)
    (format stream content)))

(defun write-numeric-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~D~%" segment))
    (format out "~%")))

;--------------------------------------------
;--------------------------------------------

(defun test-map (n)
  (progn
  (map 'array #'sqrt (make-sequence 'array n :initial-element 4))
  ()
  ))

(defun test-loop (n)
  (progn
      (let ((out (make-sequence 'array n))
          (in (make-sequence 'array n :initial-element 4))) 
     (loop for index fixnum from 0 to (- n 1)
       do (setf (aref out index) (sqrt (aref in index))))
     out)
      ()
    ))

;--------------------------------------------
;--------------------------------------------

(defun amin-generate-tests (test-count max-list-size)
    (let ( (map-results `()) (loop-results `()) )
          (loop for list-size from 1 to (+ max-list-size 1)
              do (setf map-results (append map-results  (list (amin-run-test test-count (test-map list-size)))))
           ) 
          (loop for list-size from 1 to (+ max-list-size 1)
              do (setf loop-results (append loop-results  (list (amin-run-test test-count (test-loop list-size)))))
           ) 
          (write-numeric-list "/Users/aminbenarieb/Desktop/map-tests.cvs" map-results)
          (write-numeric-list "/Users/aminbenarieb/Desktop/loop-tests.cvs" loop-results)))

(defun amin-one-test (n)
  (format t "~%~%<<Test map:>>~%")
  (time(test-map n))
  (format t "~%~%<<Test loop:>>~%")
  (time (test-loop n))
  (format t "~%~%")
  )

;--------------------------------------------
;--------------------------------------------