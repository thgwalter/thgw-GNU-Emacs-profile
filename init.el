;;; init.el --- My Emacs configuration file.

;;; Commentary:

;; no comment

;;; Code:

;; Basic configuration
(progn

  (server-start)
  
  (setq byte-compile-warnings '(cl-functions))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-verbose nil)
  (use-package diminish)
  (setq load-prefer-newer t)

  (defconst my/million (* 1000 1000))
  (setq gc-cons-threshold (* 50 my/million))
  (setq large-file-warning-threshold (* 100 my/million))

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

  (setq use-dialog-box nil))

(add-hook
 'after-init-hook
 (lambda ()
   (set-face-attribute
    'default nil
    :height 110
    :font (cond
	   ((find-font (font-spec :name "FiraCode Nerd Font"))   "FiraCode Nerd Font Light")
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
   (raise-frame)))

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


;;(visual-line-mode t)
(global-visual-line-mode t)


;; Themes
(progn
  
  ;; Use 'M-x load-theme' for the following themes.
  ;; Loading a theme is a cumulative process.
  ;; So, to erase what has been loaded so far, use 'M-x disable-all-themes'.
  
  (use-package anti-zenburn-theme)
  (use-package zenburn-theme)
  (use-package app-monochrome-themes)
  
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
  (use-package dashboard
    :config
    (dashboard-setup-startup-hook)))

(use-package treemacs
  :bind
  (:map global-map
	([f10] . treemacs)
	("C-<f10>" . treemacs-select-window))
  :custom
  '(treemacs-is-never-other-window t))

(use-package all-the-icons)
(use-package nerd-icons)
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))


;; Cursor
(progn
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
    (company-tooltip-flip-when-above t)))

;; Linting
(progn
  (use-package lsp-mode
    :hook ((cider-mode . lsp))
    :config
    (dolist (m '(cider-mode))
      (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
    (setq lsp-enable-indentation nil)
    (setq lsp-enable-completion-at-point nil)
    (setq lsp-enable-file-watchers nil)
    (setq lsp-enable-folding 1))

  (use-package lsp-ui
    :commands lsp-ui-mode))

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

  (use-package clj-refactor)

  (use-package clojure-mode
    :diminish
    :hook ((clojure-mode . aggressive-indent-mode)
	   ;;(clojure-mode . flycheck-mode)
	   (clojure-mode . paredit-mode)
	   (clojure-mode . clj-refactor-mode)
	   (clojurescript-mode . aggressive-indent-mode)
	   ;;(clojurescript-mode . flycheck-mode)
	   (clojurescript-mode . paredit-mode)
	   (clojurec-mode . aggressive-indent-mode)
	   ;;(clojurec-mode . flycheck-mode)
	   (clojurec-mode . paredit-mode)))
  
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

  (use-package babashka)

  (use-package neil
    :config 
    (setq neil-prompt-for-version-p nil
	  neil-inject-dep-to-project-p t)))

;; Project management
(progn
  (use-package projectile)
  (use-package magit))

(use-package clomacs)

(use-package zig-mode)

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4)
  (setq graphviz-dot-preview-extension "pdf")
  (setq image-use-external-converter t)
  :hook
  ((graphviz-dot-mode . paredit-mode)))

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

(if (equal system-type 'windows-nt)
    (progn
      
      (setq explicit-shell-file-name "c:/Users/Utilisateur/scoop/apps/pwsh/current/pwsh.exe")

      (defun pwsh (&optional buffer)
	"Launches a powershell in buffer *powershell* and switches to it."
	(interactive)
	(let ((buffer (or buffer "*powershell*"))
	      (powershell-prog explicit-shell-file-name))
	  (make-comint-in-buffer "shell" "*powershell*" powershell-prog)
	  (switch-to-buffer buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;

;; (Locate-user-emacs-file "test.el")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(provide 'init)
;;; init.el ends here

