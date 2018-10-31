(defpackage #:ox
  (:use #:cl)
  (:export #:digit
           #:value
           #:adapt
           #:parse
           #:build
           #:logic
           #:arity)
  (:documentation "OX - a system of binary numbers and logic"))

(in-package #:ox)

(defparameter *ox* "ox")

(defun digit (a-digit)
  "Returns the ox-digit corresponding to a binary digit."
  (declare (type bit a-digit))
  (assert (= 2 (length *ox*)))
  (char *ox* a-digit))

(defun value (a-character)
  "Returns the binary digit corresponding to an ox-digit."
  (declare (type character a-character))
  (assert (= 2 (length *ox*)))
  (position a-character *ox*))

(defun adapt (a-non-negative-integer)
  "Returns ox-number corresponding to a non-negative integer."
  (declare (type (integer 0) a-non-negative-integer))
  (with-output-to-string (str)
    (if (zerop a-non-negative-integer)
        (princ (digit 0)
               str)
        (dotimes (n (integer-length a-non-negative-integer))
          (princ (if (logbitp n a-non-negative-integer)
                     (digit 1)
                     (digit 0))
                 str)))))

(defun parse (a-string)
  "Returns the non-negative integer corresponding to an ox-number"
  (flet ((ox-fold (chr acc)
           (+ acc
              acc
              (value chr))))
    ;; FIXME: see if this can be turned into a left-fold
    (reduce #'ox-fold
            a-string
            :initial-value 0
            :from-end t)))

(defun build (an-ox-digit &rest more-ox-digits)
  "Returns ox-number of list of ox-digits"
  (let ((maybe (concatenate 'string
                            an-ox-digit
                            (values-list more-ox-digits))))
    (when (parse maybe)
      maybe)))

(defun logic (operator operands)
  "Returns ox-digit at the index of the operator computed by operands."
  (let ((out (ignore-errors
               (char operator
                     (parse operands)))))
    (string (or out (digit 0)))))

;;; TODO (maybe) should `logic' support streams? It should support
;;; streams. We'd have a way to concatenate results without relying
;;; on `build' and would be a step toward making an ox-machine. Think
;;; about this.

(defun arity (operator)
  "Returns maximum significant length of operands supported by operator."
  ;; Can't totally rely on length since an operator could have any
  ;; number of #\o's on the right, also, `o' basically ignores it's
  ;; inputs, but `x' definitely doesn't, so ¯\_(ツ)_/¯.
  (cond ((string= "o" operator)
         0)
        ((string= "x" operator)
         1)
        (t
         (values
          (ceiling (log (integer-length (parse operator)) 2))))))


;;;; Tests

;;;; Digit and Value

(assert (char= (digit 0) (char *ox* 0)))
(assert (char= (digit 1) (char *ox* 1)))
(assert (null (ignore-errors (digit 2))))

(assert (= 0 (value (char *ox* 0))))
(assert (= 1 (value (char *ox* 1))))
(assert (null (value #\z)))

;;;; Adapt

(assert (string= "o" (adapt 0)))
(assert (string= "x" (adapt 1)))

(assert (string= "ox" (adapt 2)))
(assert (string= "xx" (adapt 3)))

(assert (string= "oox" (adapt 4)))
(assert (string= "xox" (adapt 5)))
(assert (string= "oxx" (adapt 6)))
(assert (string= "xxx" (adapt 7)))

(assert (string= "xxxooxx" (adapt 103)))
(assert (string= "oxooxoxx" (adapt 210)))

;;;; Parse

(assert (= 1 (parse "x")   (parse "xo")   (parse "xoo")))
(assert (= 3 (parse "xx")  (parse "xxo")  (parse "xxoo")))
(assert (= 6 (parse "oxx") (parse "oxxo") (parse "oxxoo")))

;;;; Logic

(assert (string= "o" (logic "o" (adapt (random 100)))))
(assert (string= "o" (logic "o" (adapt (random 100)))))
(assert (string= "o" (logic "o" (adapt (random 100)))))
(assert (string= "o" (logic "o" (adapt (random 100)))))

(assert (string= "o" (logic "ox" "o")))
(assert (string= "x" (logic "ox" "x")))

(assert (string= "x" (logic "xo" "o")))
(assert (string= "o" (logic "xo" "x")))

(assert (string= "o" (logic "x"   "ox")))
(assert (string= "o" (logic "xx"  "oox")))
(assert (string= "o" (logic "xxx" "ooox")))

;;;; De Morgan's Laws

(flet ((ox-nor (operands) (logic "xooo" operands))
       (ox-nnd (operands) (logic "xxxo" operands))
       (ox-and (operands) (logic "ooox" operands))
       (ox-eqv (operands) (logic "xoox" operands))
       (ox-ior (operands) (logic "oxxx" operands)))
  (loop
     for subject in '("oo" "xo" "ox" "xx")
     never
       (assert (string= "x"
                        (ox-eqv
                         (build (ox-ior subject)
                                (ox-nor (ox-nor subject))))))
     never
       (assert (string= "x"
                        (ox-eqv
                         (build (ox-and subject)
                                (ox-nor (ox-nnd subject))))))))

;;;; Arity

(assert (= 0 (arity "o")))
(assert (= 1 (arity "x")))
(assert (= 1 (arity "ox")        (arity "xx")))
(assert (= 2 (arity "oox")       (arity "xxxx")))
(assert (= 3 (arity "oooox")     (arity "xxxxxxxx")))
(assert (= 4 (arity "oooooooox") (arity "xxxxxxxxxxxxxxxx")))
