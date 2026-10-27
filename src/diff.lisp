(in-package #:colordiff)

;;;; -- Bounded Endpoint Diffs --

(defun colordiff--line-number-width
    (old-lines new-lines &key (old-start-line 1) (new-start-line 1))
  "Return the display width needed for OLD-LINES and NEW-LINES numbers.
OLD-START-LINE and NEW-START-LINE number the first line of each side."
  (let ((largest
          (max (+ old-start-line (max 0 (1- (length old-lines))))
               (+ new-start-line (max 0 (1- (length new-lines)))))))
    (max 1 (length (princ-to-string largest)))))

(defun colordiff--line-number-cell (line-number width)
  "Return LINE-NUMBER right aligned to WIDTH."
  (format nil "~V@A" width line-number))

(defun colordiff--line-row
    (kind text span-function &key (width 1) line-number content-spans)
  "Return one numbered context, removed, or added row with a semantic gutter."
  (let* ((gutter-role
           (ecase kind
             (:context
              ':context-gutter)
             (:removed
              ':removed-gutter)
             (:added
              ':added-gutter)))
         (marker
           (ecase kind
             (:context
              " ")
             (:removed
              "-")
             (:added
              "+")))
         (gutter
           (format nil "~A ~A │ "
                   marker
                   (colordiff--line-number-cell line-number width)))
         (content-role (if (eq kind ':context) ':context ':code)))
    (cons
     (funcall span-function gutter-role gutter)
     (or content-spans
         (list (funcall span-function content-role text))))))

(defun colordiff--common-prefix-length (old-lines new-lines)
  "Return the number of equal leading lines in OLD-LINES and NEW-LINES."
  (loop for index below (min (length old-lines) (length new-lines))
        while (string= (aref old-lines index) (aref new-lines index))
        count t))

(defun colordiff--common-suffix-length (old-lines new-lines prefix-length)
  "Return equal trailing lines after PREFIX-LENGTH without overlap."
  (let ((maximum (min (- (length old-lines) prefix-length)
                      (- (length new-lines) prefix-length))))
    (loop for offset from 1 to maximum
          while (string= (aref old-lines (- (length old-lines) offset))
                         (aref new-lines (- (length new-lines) offset)))
          count t)))

(defun colordiff--block-rows
    (lines kind span-function
     &key start-line (width 1) highlighted-lines (limit 8))
  "Return bounded numbered changed LINES of KIND."
  (let* ((visible-count (min limit (length lines)))
         (omitted       (- (length lines) visible-count))
         (noun          (ecase kind
                          (:removed
                           "removed")
                          (:added
                           "added"))))
    (append
     (loop for index below visible-count
           for line-number = (+ start-line index)
           collect (colordiff--line-row
                    kind
                    (aref lines index)
                    span-function
                    :width         width
                    :line-number   line-number
                    :content-spans (and highlighted-lines
                                        (aref highlighted-lines index))))
     (when (plusp omitted)
       (let* ((line-number (+ start-line visible-count))
              (message     (format nil "… +~D more ~A line~:P" omitted noun)))
         (list
          (colordiff--line-row
           kind
           message
           span-function
           :width       width
           :line-number line-number
           :content-spans
           (list (funcall span-function ':elision message)))))))))

(defun render-diff
    (&key removed-content added-content
          removed-start-line added-start-line
          source-path syntax-language
          (syntax-highlight-p
            (not (null (or source-path syntax-language))))
          (line-limit 8)
          (sanitize-line-function #'sanitize-line)
          (span-function #'make-span))
  "Render one bounded line-numbered endpoint diff with optional highlighting.

REMOVED-CONTENT and ADDED-CONTENT are complete before-and-after documents for
one change. A NIL side denotes an absent document, so creation and removal use
the same renderer. SOURCE-PATH is language-classification metadata only and is
never opened. SYNTAX-LANGUAGE overrides path inference. Every changed row
carries a line number: an absent start line inherits the other side's
coordinates, and when both are absent the documents number from their own
first line.

The result is a list of rows. Each row is a list of values produced by
SPAN-FUNCTION from a semantic role and display text. This is an endpoint viewer,
not an LCS or patch generator: it retains one equal line around the single
middle change block after removing the common prefix and suffix."
  (check-type removed-content (or null string))
  (check-type added-content (or null string))
  (check-type removed-start-line (or null integer))
  (check-type added-start-line (or null integer))
  (check-type line-limit (integer 1))
  (check-type sanitize-line-function function)
  (check-type span-function function)
  (let* ((effective-removed-start
           (or removed-start-line added-start-line 1))
         (effective-added-start
           (or added-start-line removed-start-line 1))
         (old-source (or removed-content ""))
         (new-source (or added-content ""))
         (old-lines  (colordiff--split-lines old-source))
         (new-lines  (colordiff--split-lines new-source))
         (old-display-lines
           (colordiff--prepare-lines old-source sanitize-line-function))
         (new-display-lines
           (colordiff--prepare-lines new-source sanitize-line-function))
         (old-highlighted
           (and syntax-highlight-p
                (plusp (length old-display-lines))
                (highlight-lines
                 (colordiff--lines-source old-display-lines)
                 :language      syntax-language
                 :pathname      source-path
                 :span-function span-function)))
         (new-highlighted
           (and syntax-highlight-p
                (plusp (length new-display-lines))
                (highlight-lines
                 (colordiff--lines-source new-display-lines)
                 :language      syntax-language
                 :pathname      source-path
                 :span-function span-function)))
         (prefix-length
           (colordiff--common-prefix-length old-lines new-lines))
         (suffix-length
           (colordiff--common-suffix-length
            old-lines new-lines prefix-length))
         (width
           (colordiff--line-number-width
            old-lines
            new-lines
            :old-start-line effective-removed-start
            :new-start-line effective-added-start)))
    (if (and (= prefix-length (length old-lines))
             (= prefix-length (length new-lines)))
        (list (list (funcall span-function ':context "no textual change")))
        (let ((removed (subseq old-display-lines
                               prefix-length
                               (- (length old-lines) suffix-length)))
              (added   (subseq new-display-lines
                               prefix-length
                               (- (length new-lines) suffix-length))))
          (append
           (when (plusp prefix-length)
             (list
              (colordiff--line-row
               ':context
               (aref old-display-lines (1- prefix-length))
               span-function
               :width       width
               :line-number (+ effective-added-start prefix-length -1)
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted (1- prefix-length)))
                   (and old-highlighted
                        (aref old-highlighted (1- prefix-length)))))))
           (colordiff--block-rows
            removed
            ':removed
            span-function
            :start-line (+ effective-removed-start prefix-length)
            :width      width
            :highlighted-lines
            (and old-highlighted
                 (subseq old-highlighted
                         prefix-length
                         (- (length old-lines) suffix-length)))
            :limit      line-limit)
           (colordiff--block-rows
            added
            ':added
            span-function
            :start-line (+ effective-added-start prefix-length)
            :width      width
            :highlighted-lines
            (and new-highlighted
                 (subseq new-highlighted
                         prefix-length
                         (- (length new-lines) suffix-length)))
            :limit      line-limit)
           (when (plusp suffix-length)
             (list
              (colordiff--line-row
               ':context
               (aref old-display-lines (- (length old-lines) suffix-length))
               span-function
               :width       width
               :line-number (+ effective-added-start
                               (- (length new-lines) suffix-length))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted
                              (- (length new-lines) suffix-length)))
                   (and old-highlighted
                        (aref old-highlighted
                              (- (length old-lines) suffix-length))))))))))))

(defun render-line-row
    (kind text line-number
     &key (width (length (princ-to-string line-number)))
          content-spans
          (span-function #'make-span))
  "Return one numbered row of KIND for TEXT, as RENDER-DIFF builds it.

KIND is :CONTEXT, :REMOVED or :ADDED. LINE-NUMBER is right aligned to WIDTH
columns. CONTENT-SPANS, when supplied, replace the single span made from TEXT,
so a caller can place a styled message in a row that lines up with a diff.
TEXT is display text and is not sanitized here; WRITE-ANSI sanitizes at the
terminal boundary."
  (check-type kind (member :context :removed :added))
  (check-type text string)
  (check-type line-number (integer 0))
  (check-type width (integer 1))
  (check-type content-spans list)
  (check-type span-function function)
  (colordiff--line-row kind
                       text
                       span-function
                       :width         width
                       :line-number   line-number
                       :content-spans content-spans))
