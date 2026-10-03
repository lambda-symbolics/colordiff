(in-package #:colordiff)

;;;; -- ANSI Terminal Rendering --

(defparameter *default-style-table*
  (list
   (cons ':plain (make-style))
   (cons ':code (make-style :foreground ':cyan))
   (cons ':context (make-style :faint t))
   (cons ':elision (make-style :faint t))
   (cons ':prompt (make-style :faint t))
   (cons ':context-gutter (make-style :faint t))
   (cons ':removed-gutter (make-style :foreground ':red :bold t))
   (cons ':added-gutter (make-style :foreground ':green))
   (cons ':source-gutter (make-style :foreground ':green))
   (cons ':syntax-comment (make-style :faint t))
   (cons ':syntax-keyword (make-style :foreground ':magenta))
   (cons ':syntax-string (make-style :foreground ':green))
   (cons ':syntax-escape (make-style :foreground ':yellow))
   (cons ':syntax-number (make-style :foreground ':yellow))
   (cons ':syntax-type (make-style :foreground ':cyan))
   (cons ':syntax-function (make-style :foreground ':blue))
   (cons ':syntax-property (make-style :foreground ':cyan))
   (cons ':syntax-heading (make-style :foreground ':magenta :bold t))
   (cons ':syntax-link (make-style :foreground ':cyan :underline t)))
  "Colorist styles for Colordiff's default semantic roles.")

(defun write-ansi
    (rows &key (stream *standard-output*)
               (style-table *default-style-table*)
               (color-level (effective-color-level))
               (sanitize-text-function #'sanitize-line))
  "Write semantic ROWS safely to STREAM using ANSI colors from STYLE-TABLE.

SANITIZE-TEXT-FUNCTION is applied at this final terminal boundary, including to
spans built directly or returned by the standalone syntax helpers."
  (check-type stream stream)
  (check-type sanitize-text-function function)
  (dolist (row rows)
    (dolist (span row)
      (check-type span span)
      (let* ((style (rest (assoc (span-role span) style-table)))
             (safe-text (funcall sanitize-text-function (span-text span))))
        (check-type safe-text string)
        (write-string
         (if style
             (colorize safe-text style :level color-level)
             safe-text)
         stream)))
    (terpri stream))
  nil)

(defun ansi-string
    (rows &key (style-table *default-style-table*)
               (color-level (effective-color-level))
               (sanitize-text-function #'sanitize-line))
  "Return semantic ROWS as one safely ANSI-rendered string."
  (with-output-to-string (stream)
    (write-ansi rows
                :stream stream
                :style-table style-table
                :color-level color-level
                :sanitize-text-function sanitize-text-function)))
