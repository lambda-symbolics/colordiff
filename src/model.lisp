(in-package #:colordiff)

;;;; -- Semantic Spans and Text Preparation --

(deftype span-role ()
  "A semantic role carried by one rendered terminal span."
  '(member :plain :code :context :elision :prompt
           :context-gutter :removed-gutter :added-gutter :source-gutter
           :syntax-comment :syntax-keyword :syntax-string :syntax-escape
           :syntax-number :syntax-type :syntax-function :syntax-property
           :syntax-heading :syntax-link))

(defstruct (span
            (:constructor make-span (role text)))
  "One text fragment carrying a semantic rendering role."
  (role ':plain :type span-role :read-only t)
  (text "" :type string :read-only t))

(defun sanitize-line (line)
  "Return LINE with terminal controls represented as visible text."
  (check-type line string)
  (with-output-to-string (stream)
    (loop for character across line
          for code = (char-code character)
          do (cond
               ((char= character #\Tab)
                (write-string "\\t" stream))
               ((char= character #\Newline)
                (write-string "\\n" stream))
               ((char= character #\Return)
                (write-string "\\r" stream))
               ((= code 27)
                (write-string "\\e" stream))
               ((or (< code 32)
                    (= code 127)
                    (<= 128 code 159))
                (format stream "\\x~2,'0X" code))
               (t
                (write-char character stream))))))

(defun colordiff--split-lines (content)
  "Return CONTENT's logical lines without LF or CRLF delimiters."
  (check-type content string)
  (if (zerop (length content))
      #()
      (let ((lines nil)
            (start 0)
            (length (length content)))
        (loop
          for newline = (position #\Newline content :start start)
          for end = (or newline length)
          for logical-end = (if (and (> end start)
                                     (char= (char content (1- end)) #\Return))
                                (1- end)
                                end)
          do (push (subseq content start logical-end) lines)
          if newline
            do (setf start (1+ newline))
          else
            do (return)
          when (= start length)
            do (return))
        (coerce (nreverse lines) 'vector))))

(defun colordiff--prepare-lines (content sanitize-line-function)
  "Return CONTENT's logical lines after SANITIZE-LINE-FUNCTION."
  (check-type sanitize-line-function function)
  (let ((lines (colordiff--split-lines content)))
    (map 'vector
         (lambda (line)
           (let ((sanitized (funcall sanitize-line-function line)))
             (check-type sanitized string)
             sanitized))
         lines)))

(defun colordiff--lines-source (lines)
  "Join vector LINES with LF delimiters for syntax highlighting."
  (format nil "~{~A~^~%~}" (coerce lines 'list)))
