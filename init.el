;;; init.el --- My Emacs configuration file.

;;; Commentary:

;; no comment

;;; Code:



;; Basic configuration
(progn
  ;;(server-start)
  (setq byte-compile-warnings '(cl-functions))
  (require 'package)
  '(add-to-list 'package-archives '("nongnu"       . "https://elpa.nongnu.org/nongnu/")    t)
  '(add-to-list 'package-archives '("elpa"         . "https://elpa.gnu.org/packages/")     t)
  (add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")        t)
  '(add-to-list 'package-archives '("org"          . "https://orgmode.org/elpa/")          t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  '(unless (package-installed-p 'use-package)
     (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-verbose nil)
  (use-package diminish)
  (setq load-prefer-newer t)

  (defconst my/million (* 1000 1000))
  (setq gc-cons-threshold (* 50 my/million))
  (setq large-file-warning-threshold (* 100 my/million))

  '(when (eq system-type 'windows-nt)
     (setq w32-pass-lwindow-to-system nil)
     (setq w32-lwindow-modifier 'super)
     (setq w32-pass-rwindow-to-system nil)
     (setq w32-rwindow-modifier 'super)
     (defvar w32-pass-apps-to-system nil)
     (setq w32-apps-modifier 'hyper))

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-processes nil)
  (delete-selection-mode t)
  (setq frame-title-format
	'((:eval (if (buffer-file-name)
		     (abbreviate-file-name (buffer-file-name))
		   "%b"))))
  (size-indication-mode 0)
  (setq ring-bell-function 'ignore)
  
  (defun find-user-init ()
    "Find my Emacs config file."
    (interactive)
    (find-file user-init-file))

  (defun find-user-deps-edn ()
    "Find my Emacs config file."
    (interactive)
    (find-file "c:/Users/Utilisateur/.clojure/thgw-deps-profile/deps.edn"))



  '(setq user-full-name "Thomas Walter"
	 user-mail-address "thg.walter@free.fr")

  '(recentf-mode t)
  '(savehist-mode t)
  '(setq history-length 25)
  '(save-place-mode t)
  (setq use-dialog-box nil))

;; When using a GUI (i. e. not a terminal),
;; customize fonts frame size and position.
(;;if window-system

 add-hook 'after-init-hook
 (lambda ()   
   (set-face-attribute
    'default nil
    :height 110
    :font (cond
	   ((find-font (font-spec :name "Fira Code"))            "Fira Code Light")
	   ((find-font (font-spec :name "JetBrains Mono"))       "JetBrains Mono Light")
	   ((find-font (font-spec :name "Cascadia Code"))        "Cascadia Code")
	   ((find-font (font-spec :name "CMU Typewriter Text"))  "CMU Typewriter Text Light")
	   ((find-font (font-spec :name "Menlo"))                "Menlo")
	   ((find-font (font-spec :name "DejaVu Sans Mono"))     "DejaVu Sans Mono")
	   ((find-font (font-spec :name "Inconsolata"))          "Inconsolata")))
   (set-frame-position (selected-frame) 80 80)
   (set-frame-size (selected-frame) 110 40)
   (global-hl-line-mode t)
   (global-prettify-symbols-mode t)
   (raise-frame))
 
 ;;(set-face-background 'default "undefined")
 )



'(add-hook 'after-make-frame-functions
	   (lambda (f)
	     (set-face-attribute 'default nil :height 160 :font "Fira Code Light")
	     ;;(set-frame-position (selected-frame) 80 80)
	     ;;(set-frame-size (selected-frame) 110 40)
	     ;;(global-hl-line-mode t)
	     ;;(global-prettify-symbols-mode t)
	     ))


(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  (global-ligature-mode t))

;; Themes
(progn
  
  ;; Use 'M-x load-theme' for the following themes.
  ;; Loading a theme is a cumulative process.
  ;; So, to erase what has been loaded so far, use 'M-x disable-all-themes'.
  
  (use-package anti-zenburn-theme)
  (use-package zenburn-theme)
  '(use-package nezburn-theme)

  (defun disable-all-themes ()
    "Disable all the themes already loaded."
    (interactive)
    (dolist (theme custom-enabled-themes)
      (disable-theme theme)))

  (defvar light-theme-enabled nil)

  (defun light-theme ()
    "Load my preferred light theme."
    (interactive)
    (disable-all-themes)
    (load-theme 'anti-zenburn t)
    (setq light-theme-enabled t))
  
  (defun dark-theme ()
    "Load my preferred dark theme."
    (interactive)
    (disable-all-themes)
    (load-theme 'zenburn t)
    (setq light-theme-enabled nil))

  '(defun nez-theme ()
     "Load the nezburn theme."
     (interactive)
     (disable-all-themes)
     (load-theme 'nezburn t)
     (setq light-theme-enabled nil))

  ;; Starting dark by default.
  (dark-theme)

  ;; Then toggle if needed.
  (defun toggle-theme ()
    "Function to toggle between dark and light theme."
    (interactive)
    (if light-theme-enabled (dark-theme) (light-theme))))

;; Initial screen
(progn
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message "")
  '(use-package dashboard
     :config
     (dashboard-setup-startup-hook)
     (setq dashboard-startup-banner "c:/opt/Clojure_Logo.svg")
     ;;(setq dashboard-items-default-length 15)
     ;;(setq dashboard-footer-messages nil)
     ;;(setq dashboard-banner-logo-title nil)
     ;;(setq dashboard-startup-banner nil)
     ))

;; Moving between buffers
'(use-package windmove
   :config
   (global-set-key (kbd "C-c <left>")  'windmove-left)
   (global-set-key (kbd "C-c <right>") 'windmove-right)
   (global-set-key (kbd "C-c <up>")    'windmove-up)
   (global-set-key (kbd "C-c <down>")  'windmove-down))

(use-package treemacs
  :bind
  (:map global-map
	([f10] . treemacs)
	("C-<f10>" . treemacs-select-window))
  :custom
  '(treemacs-is-never-other-window t))

(use-package nerd-icons)
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))


;; Cursor
(progn
  '(blink-cursor-mode 0)
  '(use-package bar-cursor
     :diminish
     :init (bar-cursor-mode t))
  ;; highlight corsor when scrolling
  (use-package beacon
    :diminish
    :init (beacon-mode t)))

;; Scrolling
(scroll-bar-mode 0)

(progn
  (global-auto-revert-mode t)
  (setq tab-always-indent 'complete)
  (set-fringe-mode 10)
  ;; My screen is 4k, so...
  '(tooltip-mode t)
  (tool-bar-mode 0)
  (menu-bar-mode t)
  ;; Removes the option menu. Every option is defined in this file.
  (define-key global-map [menu-bar options] nil))

;; Line numbers
(progn
  (line-number-mode 0)
  (column-number-mode t)
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode t))))
  (dolist (mode '(org-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; When creating new files
(progn
  (defun er-auto-create-missing-dirs ()
    "Make missing parent directories automatically."
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
	(make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs))

;; Minibuffer
(progn
  (use-package icomplete
    :config
    (icomplete-mode t)
    (fido-vertical-mode t)
    (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
    (define-key icomplete-minibuffer-map (kbd "<left>")  'icomplete-backward-completions)
    :custom
    (icomplete-separator "\n")
    (icomplete-hide-common-prefix nil)
    (icomplete-in-buffer t))

  (use-package which-key
    :diminish
    :init (which-key-mode t)
    :custom (which-key-idle-delay 1.5)))

;; Completion
(progn
  '(setq hippie-expand-try-functions-list
	 '(try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-complete-file-name-partially
           try-complete-file-name
           try-expand-all-abbrevs
           try-expand-list
           try-expand-line
           try-complete-lisp-symbol-partially
           try-complete-lisp-symbol))

  (use-package company
    :diminish
    :config
    (global-company-mode)
    :custom
    (company-idle-delay 1.0)
    (company-show-numbers 0)
    (company-tooltip-limit 15)
    (company-minimum-prefix-length 2)
    (company-tooltip-align-annotations t)
    (company-tooltip-flip-when-above t))

  '(use-package company-web)
  '(use-package company-shell)
  ;;(use-package company-c-headers)

  '(use-package anakondo)
  )

(use-package charmap)
'(use-package chemtable)
'(use-package chess)
'(use-package chronometer)

;; LSP : What a piece of CRAP!
(progn

  (use-package lsp-mode
    :ensure t
    :hook (;;(clojure-mode . lsp)
           ;;(clojurec-mode . lsp)
           ;;(clojurescript-mode . lsp)
	   (cider-mode . lsp))
    :config
    (dolist (m '(;;clojure-mode
		 ;;clojurec-mode
		 ;;clojurescript-mode
		 ;;clojurex-mode
		 cider-mode))
      (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
    ;;(setq lsp-clojure-server-command '("/path/to/clojure-lsp"))
    )

  '(use-package lsp-ui
     :commands lsp-ui-mode)
  (use-package lsp-haskell)
  '(use-package lsp-java)
  '(use-package lsp-latex)
  '(use-package lsp-sonarlint)

  )

;; Linting
(progn
  (use-package flycheck
    :hook ((after-init-hook . global-flycheck-mode)))
  '(use-package flycheck-eldev)
  (use-package flycheck-clj-kondo))

;; MARKDOWN
'(progn
   (use-package markdown-mode)
   (use-package markdown-preview-mode))

;; LISP
(progn
  (use-package expand-region)

  (use-package paredit
    :diminish
    :hook ((emacs-lisp-mode                  . paredit-mode)
	   (lisp-interaction-mode            . paredit-mode)
	   (ielm-mode-hook                   . paredit-mode)
	   (lisp-mode-hook                   . paredit-mode)
	   (eval-expression-minibuffer-setup . paredit-mode)))

  (use-package paredit-menu)

  (use-package aggressive-indent
    :diminish
    :hook ((emacs-lisp-mode       . aggressive-indent-mode)
	   (lisp-interaction-mode . aggressive-indent-mode)
	   (ielm-mode             . aggressive-indent-mode)
	   (lisp-mode             . aggressive-indent-mode)))

  '(use-package eldoc
     :diminish)

  ;; Travelling through CamelCase
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
  ;; Useful for Clojure since there are Java classes.
  '(use-package subword
     :diminish)

  (use-package clj-refactor)

  (use-package clojure-mode
    :diminish
    :config
    (require 'flycheck-clj-kondo)
    :hook ((clojure-mode . aggressive-indent-mode)
	   (clojure-mode . flycheck-mode)
	   (clojure-mode . paredit-mode)
	   (clojure-mode . clj-refactor-mode)
	   (clojurescript-mode . aggressive-indent-mode)
	   (clojurescript-mode . flycheck-mode)
	   (clojurescript-mode . paredit-mode)
	   (clojurec-mode . aggressive-indent-mode)
	   (clojurec-mode . flycheck-mode)
	   (clojurec-mode . paredit-mode)))

  '(use-package clojure-ts-mode
     :diminish
     :config
     (require 'flycheck-clj-kondo)
     :hook ((clojure-ts-mode . aggressive-indent-mode)
	    (clojure-ts-mode . flycheck-mode)
	    (clojure-ts-mode . paredit-mode)
	    (clojurescript-ts-mode . aggressive-indent-mode)
	    (clojurescript-ts-mode . flycheck-mode)
	    (clojurescript-ts-mode . paredit-mode)
	    (clojurec-ts-mode . aggressive-indent-mode)
	    (clojurec-ts-mode . flycheck-mode)
	    (clojurec-ts-mode . paredit-mode)))

  '(use-package clojure-essential-ref-nov
     :init
     (setq clojure-essential-ref-default-browse-fn #'clojure-essential-ref-nov-browse))

  (use-package cider
    :diminish
    :custom
    (cider-auto-select-error-buffer nil)
    (cider-enrich-classpath t)
    (cider-eval-result-duration nil)
    (cider-eval-result-prefix "=> ")
    (cider-jump-to-compilation-error nil)
    (cider-prefer-local-resources t)
    (cider-prompt-for-symbol nil)
    (cider-repl-result-prefix " => ")
    (cider-save-file-on-load t)
    (cider-show-error-buffer nil)
    (nrepl-hide-special-buffers t)

    (cider-inject-dependencies-at-jack-in t)
    
    (nrepl-log-messages t)
    :config
    (define-key cider-repl-mode-map (kbd "C-c M-b") #'cider-repl-clear-buffer)
    (defun toggle-eval-result-duration ()
      "Function to toggle cider-eval-result-duration between nil and 'command."
      (interactive)
      (setq cider-eval-result-duration (if cider-eval-result-duration nil 'command)))
    :hook ((cider-mode      . company-mode)
	   (cider-repl-mode . paredit-mode)
	   (cider-repl-mode . company-mode))
    :bind (("C-²" . toggle-eval-result-duration)))

  ;; For Clojure CLR
  '(use-package inf-clojure)

  (use-package babashka)

  '(use-package html-to-hiccup))

;; Programming languages
;; https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
(progn
  (use-package js2-mode
    :config
    '(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    '(add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

  (use-package js2-refactor
    :config
    '(add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

  (use-package xref-js2
    :config
    ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
    ;; unbind it.
    '(define-key js-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
	      (lambda ()
		(add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
    (add-hook 'js2-minor-mode-hook
	      (lambda ()
		(add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  (use-package flycheck-color-mode-line)
  
  (use-package web-mode
    :mode (("\\.html?\\'" . web-mode)
           ("\\.erb\\'"   . web-mode)
           ("\\.hbs\\'"   . web-mode))
    :config
    (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
    '(defun web-mode-init-hook ()
       "Hooks for Web mode. Adjust indent."
       (setq web-mode-markup-indent-offset 4))
    '(add-hook 'web-mode-hook 'web-mode-init-hook)
    (setq-default flycheck-disabled-checkers
		  (append flycheck-disabled-checkers
			  '(javascript-jshint json-jsonlist)))
    ;; Enable eslint checker for web-mode
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; Enable flycheck globally
    (add-hook 'after-init-hook #'global-flycheck-mode)
    :custom
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset    2)
    (web-mode-code-indent-offset   2))


  (use-package add-node-modules-path
    :config
    (add-hook 'flycheck-mode-hook 'add-node-modules-path))

  (use-package emmet-mode
    :config
    (add-hook 'web-mode-hook 'emmet-mode))
  
  (use-package prettier-js
    :config
    (defun web-mode-init-prettier-hook ()
      (add-node-modules-path)
      (prettier-js-mode)
      (js2-minor-mode)
      (js2-refactor-mode)
      (emmet-mode))
    (add-hook 'web-mode-hook 'web-mode-init-prettier-hook))

  )

'(use-package org)
'(use-package org-modern
   :init
   (global-org-modern-mode)
   :hook ((org-mode            . org-modern-mode)
	  (org-agenda-finalize . org-modern-agenda))
   :custom
   (org-hide-emphasis-markers t)
   (org-pretty-entities t)
   (org-ellipsis "…"))

;; Global key bindings
(progn
  (global-set-key (kbd "TAB")      #'company-indent-or-complete-common)
  (global-set-key (kbd "C-x C-b")  #'ibuffer)
  (global-set-key (kbd "C-x p")    #'proced)
  (global-set-key [escape]         #'keyboard-escape-quit)
  (global-set-key [f5]             #'toggle-menu-bar-mode-from-frame)
  (global-set-key [f6]             #'toggle-tool-bar-mode-from-frame)
  (global-set-key [f8]             #'ibuffer-list-buffers)
  (global-set-key [f9]             #'find-user-init)
  (global-set-key [C-f9]           #'find-user-deps-edn)
  (global-set-key [C-f1]           #'shortdoc-display-group)
  (global-set-key [C-f3]           #'kmacro-name-last-macro)
  (global-set-key [C-f4]           #'insert-kbd-macro)
  '(global-set-key [C-f5]           #'er/expand-region)
  '(global-set-key [C-f6]           #'er/contract-region)
  '(global-set-key [f12]            #'equake-invoke)
  (global-set-key [C-f12]          #'package-refresh-contents)
  '(global-set-key (kbd "M-/")      #'hippie-expand)
  (global-set-key (kbd "C-M-<f1>") #'info-display-manual)
  (global-set-key (kbd "M-o")      #'ace-window)
  (global-set-key (kbd "C-x C-4")  #'shrink-window-horizontally)
  (global-set-key (kbd "C-x C-6")  #'enlarge-window-horizontally)
  '(global-set-key (kbd "C-x C-a")  #'accent-menu)
  )

(use-package jsdoc)

(use-package markdown-soma)

(use-package all-the-icons)

(use-package projectile)

(use-package keycast)

(use-package racket-mode)

(use-package magit)

(use-package slime)

'(use-package php-mode)

'(use-package dockerfile-mode)

'(use-package ocaml-ts-mode)

'(use-package cargo-mode)
'(use-package rust-mode)

'(use-package zig-mode)

(use-package neil
  :config 
  (setq neil-prompt-for-version-p nil
	neil-inject-dep-to-project-p t))

;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;

;; (Locate-user-emacs-file "test.el")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(provide 'init)
;;; init.el ends here

