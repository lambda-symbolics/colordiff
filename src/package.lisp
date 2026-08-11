(defpackage #:colordiff
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
   #:sanitize-line
   #:span
   #:span-p
   #:span-role
   #:span-text
   #:write-ansi))

(in-package #:colordiff)
