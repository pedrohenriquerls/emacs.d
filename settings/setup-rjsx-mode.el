;; rjxs
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (let* ((cur-line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (if (string-match "^\\( +\\)\/?> *$" cur-line)
      (let* ((empty-spaces (match-string 1 cur-line)))
        (replace-regexp empty-spaces
                        (make-string (- (length empty-spaces) sgml-basic-offset) 32)
                        nil
                        (line-beginning-position) (line-end-position))))))
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))
;; rjxs end
