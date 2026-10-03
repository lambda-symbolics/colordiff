(in-package #:colordiff)

;;;; -- Bounded Source Previews --

(defun colordiff--source-lines (content sanitize-line-function)
  "Return CONTENT's sanitized logical lines without trailing blank lines."
  (let* ((lines (colordiff--prepare-lines content sanitize-line-function))
         (end   (length lines)))
    (loop while (and (plusp end)
                     (zerop (length (aref lines (1- end)))))
          do (decf end))
    (subseq lines 0 end)))

(defun colordiff--source-row (spans span-function)
  "Return one source row holding SPANS behind the source gutter."
  (cons (funcall span-function ':source-gutter "│ ") spans))

(defun render-source
    (content &key source-path syntax-language
                  (syntax-highlight-p
                    (not (null (or source-path syntax-language))))
                  (line-limit 8)
                  prompt
                  (sanitize-line-function #'sanitize-line)
                  (span-function #'make-span))
  "Render CONTENT as a bounded, unnumbered source preview under one gutter.

Trailing blank lines are not shown. At most LINE-LIMIT lines are rendered; the
rest are summarized by one elision row. SOURCE-PATH, SYNTAX-LANGUAGE and
SYNTAX-HIGHLIGHT-P select highlighting exactly as in RENDER-DIFF.

PROMPT, when supplied, is shown before the first line in the :PROMPT role and
later lines are indented by its width, as in a shell transcript. Empty CONTENT
renders no rows, or a single prompt row when PROMPT is supplied.

The result uses the same row representation as RENDER-DIFF."
  (check-type content string)
  (check-type line-limit (integer 1))
  (check-type prompt (or null string))
  (check-type sanitize-line-function function)
  (check-type span-function function)
  (let* ((lines (colordiff--source-lines content sanitize-line-function))
         (highlighted
           (and syntax-highlight-p
                (plusp (length lines))
                (highlight-lines (colordiff--lines-source lines)
                                 :language      syntax-language
                                 :pathname      source-path
                                 :span-function span-function)))
         (visible-count (min line-limit (length lines)))
         (omitted       (- (length lines) visible-count))
         (indentation   (and prompt
                             (make-string (length prompt)
                                          :initial-element #\Space))))
    (append
     (if (and prompt (zerop (length lines)))
         (list (colordiff--source-row
                (list (funcall span-function ':prompt prompt))
                span-function))
         (loop for index below visible-count
               for leader = prompt then indentation
               collect (colordiff--source-row
                        (append
                         (when leader
                           (list (funcall span-function ':prompt leader)))
                         (or (and highlighted (aref highlighted index))
                             (list (funcall span-function
                                            ':code
                                            (aref lines index)))))
                        span-function)))
     (when (plusp omitted)
       (list (colordiff--source-row
              (list (funcall span-function
                             ':elision
                             (format nil "… +~D more line~:P" omitted)))
              span-function))))))
