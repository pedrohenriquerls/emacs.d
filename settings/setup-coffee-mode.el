(require 'coffee-mode)
(add-to-list 'auto-mode-alist '(".*\.coffee\'" . coffee-mode))
(add-to-list 'auto-mode-alist '(".*\.js.coffee\'" . coffee-mode))
(add-to-list 'auto-mode-alist '(".*\.js.coffee.erb\'" . coffee-mode))
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables
 '(coffee-tab-width 2)
 '(coffee-args-compile '("-c" "--no-header" "--bare")))
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))

(add-hook 'coffee-mode-hook
          '(lambda ()
             (flycheck-mode 1)))


(provide 'setup-coffee-mode)
