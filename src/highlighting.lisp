(in-package #:colordiff)

;;;; -- Syntax Highlighting --

(defun colordiff--syntax-role (category)
  "Map one ColorLisp CATEGORY to a stable semantic span role."
  (case category
    (:comment
     ':syntax-comment)
    ((:keyword :macro)
     ':syntax-keyword)
    (:string
     ':syntax-string)
    ((:escape :special)
     ':syntax-escape)
    ((:number :constant)
     ':syntax-number)
    ((:type :namespace :builtin)
     ':syntax-type)
    ((:function :method)
     ':syntax-function)
    ((:property :attribute)
     ':syntax-property)
    (:heading
     ':syntax-heading)
    (:link
     ':syntax-link)
    (otherwise
     ':plain)))

(defun colordiff--highlight-segments (source language pathname)
  "Return ColorLisp segments for SOURCE, or NIL."
  (handler-case
      (let ((resolved (or language
                          (and pathname
                               (language-detect pathname :source source)))))
        (and resolved (highlight-segments source :language resolved)))
    (colorlisp-error ()
      nil)))

(define-condition colordiff--overlay-size-error (error)
  ((expected :initarg :expected :documentation "The source length in characters.")
   (actual   :initarg :actual   :documentation "The number of entries in the overlay vector."))
  (:documentation "The role overlay did not provide one entry per source character.")
  (:report (lambda (condition stream)
             (format stream "The role overlay has ~D entries for ~D source characters."
                     (slot-value condition 'actual) (slot-value condition 'expected)))))

(defun colordiff--overlay-roles (source role-overlay-function)
  "Get one role override per character of SOURCE from ROLE-OVERLAY-FUNCTION."
  (when role-overlay-function
    (let ((roles (funcall role-overlay-function source)))
      (check-type roles vector)
      (unless (= (length roles) (length source))
        (error 'colordiff--overlay-size-error :expected (length source) :actual (length roles)))
      roles)))

(defun colordiff--fragment-spans
    (role text span-function &key (start 0) (end (length text)) roles (offset 0))
  "Construct spans for TEXT from START to END, with TEXT beginning at OFFSET in ROLES."
  (if (and roles (< (+ offset start) (length roles)) (< start end))
      (flet ((role-at (index)
               "Return the override at INDEX or the fragment's original role."
               (or (and (< (+ offset index) (length roles))
                        (aref roles (+ offset index)))
                   role)))
        (loop while (< start end)
              for current = (role-at start)
              for run-end = (loop for index from (1+ start) below end
                                  unless (eq current (role-at index)) return index
                                  finally (return end))
              collect (funcall span-function current (subseq text start run-end))
              do (setf start run-end)))
      (list (funcall span-function role
                     (if (and (zerop start) (= end (length text)))
                         text
                         (subseq text start end))))))

(defun highlight-spans
    (source &key language pathname role-overlay-function (span-function #'make-span))
  "Return SOURCE as syntax-highlighted spans, or NIL.

PATHNAME is classification metadata and is never opened. ROLE-OVERLAY-FUNCTION,
if provided, receives SOURCE and returns a vector of the same length. At each
zero-based character offset, NIL keeps the syntax role and a non-NIL entry
replaces it. SPAN-FUNCTION receives each final role and text fragment. Use a
custom SPAN-FUNCTION for roles outside the built-in palette."
  (check-type source string)
  (check-type role-overlay-function (or null function))
  (check-type span-function function)
  (let ((segments (colordiff--highlight-segments source language pathname)))
    (when segments
      (loop with roles = (colordiff--overlay-roles source role-overlay-function)
            with offset = 0
            for segment in segments
            for text = (segment-text segment)
            for role = (colordiff--syntax-role (segment-category segment))
            append (colordiff--fragment-spans role text span-function
                                              :roles roles :offset offset)
            do (incf offset (length text))))))

(defun colordiff--segments->lines (segments span-function &key roles)
  "Convert ColorLisp SEGMENTS into a vector of styled rows, applying ROLES to source text."
  (let ((rows    nil)
        (current nil)
        (offset  0))
    (labels ((emit (role text start end)
               "Append TEXT between START and END to the current row."
               (when (< start end)
                 (setf current
                       (nreconc (colordiff--fragment-spans role text span-function
                                                          :start start :end end
                                                          :roles roles :offset offset)
                                current))))

             (finish-row ()
               "Finish the current row and begin another."
               (push (nreverse current) rows)
               (setf current nil)))
      (dolist (segment segments)
        (let ((text  (segment-text segment))
              (role  (colordiff--syntax-role (segment-category segment)))
              (start 0))
          (loop for newline = (position #\Newline text :start start)
                while newline
                do (emit role text start newline)
                   (finish-row)
                   (setf start (1+ newline))
                finally (emit role text start (length text)))
          (incf offset (length text))))
      (finish-row))
    (coerce (nreverse rows) 'vector)))

(defun highlight-lines
    (source &key language pathname (span-function #'make-span))
  "Return SOURCE as syntax-highlighted rows, or NIL without a language."
  (check-type source string)
  (check-type span-function function)
  (let ((segments (colordiff--highlight-segments source language pathname)))
    (and segments
         (colordiff--segments->lines segments span-function))))
