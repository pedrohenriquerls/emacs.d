(require 'coffee-mode)
(add-to-list 'auto-mode-alist '(".*\.coffee\'" . coffee-mode))
(add-to-list 'auto-mode-alist '(".*\.js.coffee\'" . coffee-mode))
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables '(coffee-tab-width 2))
