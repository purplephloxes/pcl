(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*)) ;; so clearly push is appending to a list

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd))) ;; wtf is that format

;; ~a  -> aesthetic
;; ~Xt -> tabulating, move to Xth col
;; ~%  -> new line, basically \n
;; ~{  -> consume a list and loop over it
;; ~}  -> last part of the loop

;; so one could write it as...?
(defun dump-db-alt ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; oh yea, the *x* just means it's a global var

(defun prompt-for-cd ()
  (make-cd
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Rating") :junk-allowed t) -1)
        (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: "))
            (return))))
;; so that's how you loop

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))
;; saves it as a plist, so presumably reading in would just be eval?

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
;; ahh, so no eval but you read in
;;    since read just uses the repl
;;    aka arbitrary code execution
;; and :direction defaults to input
;; oh and setf is the main assignment op
