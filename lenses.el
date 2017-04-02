(require 'rx)

(defun extract-code (name)
  "Where name has the form foo.bar.baz"
  (with-temp-buffer
    (insert-file-contents-literally "test/Main.hs")

    (let ((parts (split-string name "\\.")))
      (while parts
        (re-search-forward
         (rx-to-string `(: word-start ,(if (cdr parts) "describe" "it")
                           space
                           ?\" ,(car parts) ?\")))
        (forward-line)
        (setq parts (cdr parts))))

    (let ((beg (point)))
      (forward-paragraph)
      (let ((str (buffer-substring-no-properties beg (point))))
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (let ((width (skip-chars-forward " ")))
            (goto-char (point-min))
            (while (not (eobp))
              (delete-char width)
              (forward-line)))
          (buffer-string))))))

(defun extract-code-blocks ()
  (goto-char (point-min))
  (while (re-search-forward "^### \\(.+\\)$" nil t)
    (let ((name (match-string 1))
          (line (line-number-at-pos (match-end 0))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "#+begin_src haskell" ?\n)
      (condition-case err
          (insert (extract-code name))
        (error
         (error "Failed to locate test for %s: %s" name err)))
      (insert "#+end_src"))))
