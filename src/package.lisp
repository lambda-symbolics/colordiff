(defpackage #:colordiff
  (:documentation "Render syntax-highlighted diffs and source previews as
semantic rows and ANSI text.")
  (:use #:cl)
  (:import-from #:cl-colorist
                #:colorize
                #:effective-color-level
                #:make-style)
  (:import-from #:colorlisp
                #:colorlisp-error
                #:highlight-segments
                #:language-detect
                #:segment-category
                #:segment-text)
  (:export
   #:*default-style-table*
   #:ansi-string
   #:highlight-lines
   #:highlight-spans
   #:make-span
   #:render-diff
   #:render-line-row
   #:render-source
   #:sanitize-line
   #:span
   #:span-p
   #:span-role
   #:span-text
   #:write-ansi))

(in-package #:colordiff)
