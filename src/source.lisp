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
  "Return one source row containing SPANS behind the source gutter."
  (cons (funcall span-function ':source-gutter "│ ") spans))

(defun render-source
    (content &key source-path syntax-language
                  (syntax-highlight-p
                    (not (null (or source-path syntax-language))))
                  (line-limit 8)
                  prompt
                  role-overlay-function
                  (sanitize-line-function #'sanitize-line)
                  (span-function #'make-span))
  "Render CONTENT as a bounded, unnumbered source preview under one gutter.

Trailing blank lines are not shown. At most LINE-LIMIT lines are rendered; the
rest are summarized by one elision row.

SOURCE-PATH, SYNTAX-LANGUAGE and SYNTAX-HIGHLIGHT-P select highlighting exactly
the same way as in RENDER-DIFF.

ROLE-OVERLAY-FUNCTION works as in HIGHLIGHT-SPANS, but receives only the visible,
sanitized source lines joined with LF. It can override both syntax and :CODE
roles, without affecting the gutter, prompt, indentation, or elision row.

PROMPT, when supplied, is shown before the first line in the :PROMPT role and
later lines are indented by its width, as in a shell transcript.

Empty CONTENT does not render any rows, or a single prompt row when PROMPT is supplied.

The result uses the same row representation as RENDER-DIFF."
  (check-type content string)
  (check-type line-limit (integer 1))
  (check-type prompt (or null string))
  (check-type role-overlay-function (or null function))
  (check-type sanitize-line-function function)
  (check-type span-function function)
  (let* ((lines         (colordiff--source-lines content sanitize-line-function))
         (visible-count (min line-limit (length lines)))
         (omitted       (- (length lines) visible-count))
         (source        (when (and syntax-highlight-p (plusp visible-count))
                          (colordiff--lines-source lines)))
         (roles         (when (and role-overlay-function (plusp visible-count))
                          (colordiff--overlay-roles
                           (or (and (zerop omitted) source)
                               (colordiff--lines-source (subseq lines 0 visible-count)))
                           role-overlay-function)))
         (segments      (when source
                          (colordiff--highlight-segments source syntax-language source-path)))
         (highlighted   (when segments
                          (colordiff--segments->lines segments span-function :roles roles)))
         (indentation   (and prompt
                             (make-string (length prompt)
                                          :initial-element #\Space))))
    (append
     (if (and prompt (zerop (length lines)))
         (list (colordiff--source-row
                (list (funcall span-function ':prompt prompt))
                span-function))
         (loop with offset = 0
               for index below visible-count
               for line = (aref lines index)
               for leader = prompt then indentation
               collect (colordiff--source-row
                        (append
                         (when leader
                           (list (funcall span-function ':prompt leader)))
                         (or (and highlighted (aref highlighted index))
                             (colordiff--fragment-spans ':code line span-function
                                                       :roles roles :offset offset)))
                        span-function)
               do (incf offset (1+ (length line)))))
     (when (plusp omitted)
       (list (colordiff--source-row
              (list (funcall span-function
                             ':elision
                             (format nil "… +~D more line~:P" omitted)))
              span-function))))))
