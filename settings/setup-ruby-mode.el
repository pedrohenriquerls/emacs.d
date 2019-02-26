(require 'ruby-mode)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq rbenv-installation-dir "/usr/local")
             (global-rbenv-mode 1)
             (rbenv-use-corresponding)
             (setq flycheck-checker 'ruby-rubocop)
	     (setq flycheck-checker 'reek)
	     (setq flycheck-checker 'fasterer)
             (flycheck-mode 1)))

(provide 'setup-ruby-mode)
