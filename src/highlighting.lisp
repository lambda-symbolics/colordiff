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
  "Return ColorLisp segments for SOURCE, or NIL without a language."
  (handler-case
      (let ((resolved (or language
                          (and pathname
                               (language-detect pathname :source source)))))
        (and resolved (highlight-segments source :language resolved)))
    (colorlisp-error ()
      nil)))

(defun highlight-spans
    (source &key language pathname (span-function #'make-span))
  "Return SOURCE as syntax-highlighted spans, or NIL without a language.

PATHNAME is classification metadata and is never opened. SPAN-FUNCTION receives
one semantic role and one text fragment, allowing callers to use their own
styled-span representation."
  (check-type source string)
  (check-type span-function function)
  (let ((segments (colordiff--highlight-segments source language pathname)))
    (and segments
         (mapcar (lambda (segment)
                   (funcall span-function
                            (colordiff--syntax-role
                             (segment-category segment))
                            (segment-text segment)))
                 segments))))

(defun colordiff--segments->lines (segments span-function)
  "Convert ColorLisp SEGMENTS into a vector of styled rows."
  (let ((rows    nil)
        (current nil))
    (labels ((emit (role text start end)
               "Append TEXT between START and END to the current row."
               (when (< start end)
                 (push (funcall span-function role (subseq text start end))
                       current)))

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
                finally (emit role text start (length text)))))
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
