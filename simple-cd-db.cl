;; following ch3 in pcl
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

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))
;; great, but we can generalize to select!

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
;; no #' since we want to use VAR selector-fn, not FUN selector-fn
;; and this just does remove-if-not selector-fn onto *db*

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
;; returns the lambda. but still, if we do it for everything
;; we end up with duplication of effort & inelegance

;; inspired by SQL
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
;; regarding ripped-p, that's just about if they passed in ripped
;;   since otherwise ripped will default to nil and thus it'll think
;;   that we just want not-ripped cds!
;; oh and &key means that we can pass in by specific arg,
;;   like how in C/imperatives we can do f(x=3);

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar ;; maps over a list
                #'(lambda (row)
                    (when (funcall selector-fn row)
                      (if title (setf (getf row :title) title))
                      (if artist (setf (getf row :artist) artist))
                      (if rating (setf (getf row :rating) rating))
                      (if ripped-p (setf (getf row :ripped) ripped)))
                    row) *db*)))
;; oh yea, and apparently setf can do a bunch of stuff

(defun remove-row (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
;; remove-if complements remove-if-not
;; doesn't modify, but we passed it back into *db*

;; so all that if whatever equal getf is annoying and inefficent
;; so we can use macros
;; we can use ' to avoid evaluation, or we can use ` to stop eval
;;   UNLESS the expression is preceded by a ,
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
;; and we can loop
(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))
;; and i have no idea how that works, but I do know pop
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
;; ,@ just splices the following list into the enclosing list
;; `(and ,(list 1 2 3))   ==> (AND (1 2 3))
;; `(and ,@(list 1 2 3))  ==> (AND 1 2 3)
;; oh and &rest is like varargs in C or * params in python

;; yea idk
