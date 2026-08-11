(defpackage #:colordiff/tests
  (:use #:cl)
  (:import-from #:colordiff
                #:ansi-string
                #:highlight-lines
                #:highlight-spans
                #:make-span
                #:render-diff
                #:span-p
                #:span-role
                #:span-text))

(in-package #:colordiff/tests)

(defvar *test-count* 0
  "The number of assertions run by the current test invocation.")

(defun test-assert (value description)
  "Record one assertion and signal an error unless VALUE is true."
  (incf *test-count*)
  (unless value
    (error "Test failed: ~A" description))
  nil)

(defun test-row-text (row)
  "Return the concatenated text carried by semantic ROW spans."
  (with-output-to-string (stream)
    (dolist (span row)
      (write-string (span-text span) stream))))

(defun test-find-span (role text row)
  "Return a span in ROW matching ROLE and TEXT."
  (find-if (lambda (span)
             (and (eq (span-role span) role)
                  (string= (span-text span) text)))
           row))

(defun test-rows-contain-role-p (rows role)
  "Return true when any span in ROWS carries ROLE."
  (loop for row in rows
        thereis (find role row :key #'span-role)))

(defun test-basic-diff ()
  "Test numbered removed and added rows without syntax highlighting."
  (let ((rows
          (render-diff
           :removed-content "old value"
           :added-content "new value"
           :removed-start-line 41
           :added-start-line 41
           :syntax-highlight-p nil)))
    (test-assert (= 2 (length rows))
                 "a one-line replacement renders two rows")
    (test-assert
     (and (test-find-span ':removed-gutter "- 41 │ " (first rows))
          (test-find-span ':code "old value" (first rows))
          (test-find-span ':added-gutter "+ 41 │ " (second rows))
          (test-find-span ':code "new value" (second rows)))
     "numbered replacement rows carry semantic gutters and code")))

(defun test-context-and-coordinates ()
  "Test one leading and trailing context row with exact coordinates."
  (let ((rows
          (render-diff
           :removed-content (format nil "head~%old~%tail")
           :added-content (format nil "head~%new~%tail")
           :removed-start-line 10
           :added-start-line 10
           :syntax-highlight-p nil)))
    (test-assert
     (equal (mapcar #'test-row-text rows)
            '("  10 │ head" "- 11 │ old" "+ 11 │ new" "  12 │ tail"))
     "a middle replacement retains one correctly numbered context line per side")))

(defun test-unknown-coordinates ()
  "Test absent source coordinates remain absent."
  (let ((rows
          (render-diff
           :removed-content "old"
           :added-content "new"
           :syntax-highlight-p nil)))
    (test-assert
     (equal (mapcar #'test-row-text rows)
            '("- │ old" "+ │ new"))
     "unknown coordinates produce unnumbered gutters")))

(defun test-logical-line-endings ()
  "Test CRLF input and meaningful trailing blank logical rows."
  (let ((crlf
          (render-diff
           :removed-content
           (format nil "before~C~%old~C~%after~C~%"
                   #\Return #\Return #\Return)
           :added-content
           (format nil "before~C~%new~C~%after~C~%"
                   #\Return #\Return #\Return)
           :removed-start-line 1
           :added-start-line 1
           :syntax-highlight-p nil))
        (trailing
          (render-diff
           :added-content (format nil "first~%~%")
           :added-start-line 1
           :syntax-highlight-p nil))
        (newline-only
          (render-diff
           :added-content (string #\Newline)
           :added-start-line 1
           :syntax-highlight-p nil)))
    (test-assert
     (equal (mapcar #'test-row-text crlf)
            '("  1 │ before" "- 2 │ old" "+ 2 │ new" "  3 │ after"))
     "CRLF delimiters do not enter rendered content")
    (test-assert
     (equal (mapcar #'test-row-text trailing)
            '("+ 1 │ first" "+ 2 │ "))
     "an extra final newline preserves one blank logical row")
    (test-assert
     (equal (mapcar #'test-row-text newline-only)
            '("+ 1 │ "))
     "a newline-only document contains one blank logical row")))

(defun test-no-change ()
  "Test equal before-and-after documents report no textual change."
  (let ((rows
          (render-diff
           :removed-content (format nil "same~%")
           :added-content (format nil "same~%")
           :syntax-highlight-p nil)))
    (test-assert
     (and (= 1 (length rows))
          (test-find-span ':context "no textual change" (first rows)))
     "equal documents render one semantic no-change row")))

(defun test-bounds ()
  "Test independent removed and added line bounds and elisions."
  (let ((rows
          (render-diff
           :removed-content (format nil "a~%b~%c~%d")
           :added-content (format nil "w~%x~%y~%z")
           :removed-start-line 1
           :added-start-line 1
           :line-limit 2
           :syntax-highlight-p nil)))
    (test-assert
     (equal (mapcar #'test-row-text rows)
            '("- 1 │ a"
              "- 2 │ b"
              "- 3 │ … +2 more removed lines"
              "+ 1 │ w"
              "+ 2 │ x"
              "+ 3 │ … +2 more added lines"))
     "each side is bounded independently with the next exact line number")
    (test-assert (= 2 (count ':elision rows
                              :key (lambda (row)
                                     (and (find ':elision row :key #'span-role)
                                          ':elision))))
                 "bounded blocks carry semantic elision spans")))

(defun test-sanitization ()
  "Test visible terminal safety and caller-supplied line sanitization."
  (let* ((escape (code-char 27))
         (unsafe (format nil "~C[31mred~C[0m" escape escape))
         (safe-rows
           (render-diff
            :added-content unsafe
            :syntax-highlight-p nil))
         (control-change
           (render-diff
            :removed-content "red"
            :added-content unsafe
            :syntax-highlight-p nil))
         (custom-rows
           (render-diff
            :added-content (format nil "one~%two")
            :added-start-line 1
            :syntax-highlight-p nil
            :sanitize-line-function #'string-upcase)))
    (test-assert
     (string= "+ │ \\e[31mred\\e[0m"
              (test-row-text (first safe-rows)))
     "default sanitization visibly represents embedded terminal controls")
    (test-assert
     (= 2 (length control-change))
     "raw text differences remain visible when safe display text differs only by controls")
    (test-assert
     (equal (mapcar #'test-row-text custom-rows)
            '("+ 1 │ ONE" "+ 2 │ TWO"))
     "a caller may replace the line sanitizer")))

(defun test-syntax-highlighting ()
  "Test ColorLisp highlighting, explicit disabling, and path fallback."
  (let ((highlighted
          (render-diff
           :removed-content "(defun old-answer () 41)"
           :added-content "(defun new-answer () 42)"
           :source-path "answer.lisp"))
        (disabled
          (render-diff
           :removed-content "(defun old-answer () 41)"
           :added-content "(defun new-answer () 42)"
           :source-path "missing/never-opened.lisp"
           :syntax-highlight-p nil))
        (unknown
          (render-diff
           :removed-content "old plain"
           :added-content "new plain"
           :source-path "answer.unknown")))
    (test-assert
     (and (test-rows-contain-role-p highlighted ':syntax-keyword)
          (test-rows-contain-role-p highlighted ':syntax-function)
          (test-rows-contain-role-p highlighted ':syntax-number))
     "recognized source paths produce syntax roles")
    (test-assert
     (and (not (test-rows-contain-role-p disabled ':syntax-keyword))
          (test-rows-contain-role-p disabled ':code))
     "explicitly disabled highlighting never consults source metadata")
    (test-assert
     (and (not (test-rows-contain-role-p unknown ':syntax-keyword))
          (test-rows-contain-role-p unknown ':code))
     "unknown source paths degrade to ordinary code spans")
    (test-assert
     (and (highlight-spans "(list 1)" :pathname "form.lisp")
          (highlight-lines "(list 1)" :pathname "form.lisp"))
     "standalone syntax helpers expose span and row forms")))

(defun test-custom-span-function ()
  "Test applications can receive their own span representation directly."
  (let ((rows
          (render-diff
           :removed-content "old"
           :added-content "new"
           :syntax-highlight-p nil
           :span-function #'cons)))
    (test-assert
     (equal rows
            '(((:removed-gutter . "- │ ") (:code . "old"))
              ((:added-gutter . "+ │ ") (:code . "new"))))
     "SPAN-FUNCTION controls the exact returned row representation")))

(defun test-ansi-rendering ()
  "Test plain, colored, and terminal-safe serialization."
  (let* ((rows
           (render-diff
            :removed-content "old"
            :added-content "new"
            :syntax-highlight-p nil))
         (plain (ansi-string rows :color-level ':none))
         (colored (ansi-string rows :color-level ':basic))
         (escape (code-char 27))
         (bell (code-char 7))
         (payload
           (format nil
                   "~C]52;c;secret~C~C]8;;https://example.invalid~Clink~C]8;;~C~C[31mred~%next"
                   escape bell escape bell escape bell escape))
         (direct
           (ansi-string
            (list (list (make-span ':code payload)))
            :color-level ':none))
         (highlighted
           (ansi-string
            (list (highlight-spans
                   (format nil ";; ~A" payload)
                   :pathname "unsafe.lisp"))
            :color-level ':none)))
    (test-assert (string= plain (format nil "- │ old~%+ │ new~%"))
                 "the :none color level writes plain terminal text")
    (test-assert (find escape colored)
                 "the :basic color level writes trusted ANSI controls")
    (test-assert
     (and (not (find escape direct))
          (not (find bell direct))
          (= 1 (count #\Newline direct))
          (search "\\e]52" direct)
          (search "\\e]8" direct)
          (search "\\x07" direct)
          (search "\\e[31m" direct)
          (search "\\nnext" direct))
     "the terminal boundary neutralizes CSI, OSC, BEL, and embedded newlines")
    (test-assert
     (and (not (find escape highlighted))
          (not (find bell highlighted))
          (= 1 (count #\Newline highlighted)))
     "standalone highlighted spans cannot bypass terminal sanitization")))

(defun test-invalid-bound ()
  "Test a nonpositive line limit is rejected."
  (test-assert
   (handler-case
       (progn
         (render-diff :added-content "new" :line-limit 0)
         nil)
     (type-error ()
       t))
   "LINE-LIMIT must be positive"))

(defun run-tests ()
  "Run all Colordiff tests and return NIL after success."
  (setf *test-count* 0)
  (mapc #'funcall
        '(test-basic-diff
          test-context-and-coordinates
          test-unknown-coordinates
          test-logical-line-endings
          test-no-change
          test-bounds
          test-sanitization
          test-syntax-highlighting
          test-custom-span-function
          test-ansi-rendering
          test-invalid-bound))
  (format t "~&~D Colordiff tests passed.~%" *test-count*)
  nil)
