; list of packages
(setq package-list '(
  highlight-indent-guides
  xclip
  highlight-indentation
  yaml-mode
  go-mode
  neotree
  web-mode
  coffee-mode
  flymake-ruby
  js2-refactor
  auto-complete
  fzf
  spacemacs-theme
  ruby-mode
  rbenv
  inf-ruby
  flycheck
  flycheck-pos-tip
  string-edit
  visual-regexp
  wgrep
  whitespace-cleanup-mode
  dockerfile-mode
  smartparens
  markdown-mode
  yasnippet
  flx
  flx-ido
  ido-at-point
  ido-completing-read+
  ido-vertical-mode
  exec-path-from-shell
))

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(ac-config-default)
;; Set up load path
(add-to-list 'load-path settings-dir)
;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `(("/tmp/.*" ,temporary-file-directory t)))

;; Don't write lock-files, I'm the only one here
(setq create-lockfiles nil)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Hiligh indentation
(require 'highlight-indent-guides)
(setq highlight-indent-guides-auto-odd-face-perc 14)
(setq highlight-indent-guides-auto-even-face-perc 15)
(setq highlight-indent-guides-auto-character-face-perc 20)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)
;(set-face-background 'highlight-indent-guides-odd-face "darkgray")
;(set-face-background 'highlight-indent-guides-even-face "dimgray")
;(set-face-foreground 'highlight-indent-guides-character-face "dimgray")
(setq highlight-indent-guides-character ?\|)

;; Set path to emacs view
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq neo-smart-open t)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
; Copy from clipboard
(defun copy-from-osx ()
(shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
(let ((process-connection-type nil))
(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
(process-send-string proc text)
(process-send-eof proc))))

;(setq interprogram-cut-function 'paste-to-osx)
;(setq interprogram-paste-function 'copy-from-osx)
(global-linum-mode t)
;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
;(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'shell '(require 'setup-shell))
(eval-after-load 'flycheck '(require 'setup-flycheck))
(require 'neotree)
(require 'setup-rgrep)
(require 'setup-yasnippet)
;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          js-mode-hook
          java-mode
	  coffee-mode
          ruby-mode
          web-mode
          go-mode
          markdown-mode)
  (add-hook it 'turn-on-smartparens-mode))
;; Set tab-width size
(setq css-indent-offset 2)
(setq tab-width 2) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
;; Language specific setup files
(eval-after-load 'web-mode '(require 'setup-web-mode))
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))
(eval-after-load 'coffee-mode '(require 'setup-coffee-mode))
(eval-after-load 'go-mode '(require 'setup-go-mode))
(eval-after-load 'yaml-mode '(require 'setup-yaml-mode))
;; Load stuff on demand
(autoload 'auto-complete-mode "auto-complete" nil t)
(autoload 'xclip-mode "xclip" nil t)
; Load theme
(load-theme 'spacemacs-dark t)
;; Setup environment variables from the user's shell.
;(when is-mac
;  (require-package 'exec-path-from-shell)
;  (exec-path-from-shell-initialize))
;; Misc
;(require 'my-misc)
;(when is-mac (require 'mac))
; Shortcuts
(global-set-key (kbd "C-p") 'fzf-git)
(global-set-key (kbd "C-u") 'neotree-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (auto-complete yaml-mode whitespace-cleanup-mode wgrep web-mode visual-regexp string-edit spacemacs-theme smartparens rbenv neotree markdown-mode js2-refactor inf-ruby ido-vertical-mode ido-completing-read+ ido-at-point highlight-indentation highlight-indent-guides go-mode fzf flymake-ruby flycheck-pos-tip flx-ido exec-path-from-shell dockerfile-mode coffee-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
