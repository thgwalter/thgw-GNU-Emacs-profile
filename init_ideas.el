;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(server-start)
'(add-to-list 'package-archives '("nongnu"       . "https://elpa.nongnu.org/nongnu/")    t)
'(add-to-list 'package-archives '("elpa"         . "https://elpa.gnu.org/packages/")     t)
'(add-to-list 'package-archives '("org"          . "https://orgmode.org/elpa/")          t)

'(when (eq system-type 'windows-nt)
   (setq w32-pass-lwindow-to-system nil)
   (setq w32-lwindow-modifier 'super)
   (setq w32-pass-rwindow-to-system nil)
   (setq w32-rwindow-modifier 'super)
   (defvar w32-pass-apps-to-system nil)
   (setq w32-apps-modifier 'hyper))

'(setq user-full-name "Thomas Walter"
       user-mail-address "thg.walter@free.fr")

'(recentf-mode t)
'(savehist-mode t)
'(setq history-length 25)
'(save-place-mode t)

(set-face-background 'default "undefined")

'(add-hook 'after-make-frame-functions
	   (lambda (f)
	     (set-face-attribute 'default nil :height 160 :font "Fira Code Light")
	     ;;(set-frame-position (selected-frame) 80 80)
	     ;;(set-frame-size (selected-frame) 110 40)
	     ;;(global-hl-line-mode t)
	     ;;(global-prettify-symbols-mode t)
	     ))

'(use-package nezburn-theme)

'(defun nez-theme ()
   "Load the nezburn theme."
   (interactive)
   (disable-all-themes)
   (load-theme 'nezburn t)
   (setq light-theme-enabled nil))


'(use-package dashboard
   :config
   (dashboard-setup-startup-hook)
   (setq dashboard-startup-banner "c:/opt/Clojure_Logo.svg")
   ;;(setq dashboard-items-default-length 15)
   ;;(setq dashboard-footer-messages nil)
   ;;(setq dashboard-banner-logo-title nil)
   ;;(setq dashboard-startup-banner nil)
   )

;; Moving between buffers
'(use-package windmove
   :config
   (global-set-key (kbd "C-c <left>")  'windmove-left)
   (global-set-key (kbd "C-c <right>") 'windmove-right)
   (global-set-key (kbd "C-c <up>")    'windmove-up)
   (global-set-key (kbd "C-c <down>")  'windmove-down))

'(blink-cursor-mode 0)
'(use-package bar-cursor
   :diminish
   :init (bar-cursor-mode t))

;; My screen is 4k, so...
'(tooltip-mode t)

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


'(use-package company-web)
'(use-package company-shell)
'(use-package company-c-headers)
'(use-package anakondo)
'(use-package charmap)
'(use-package chemtable)
'(use-package chess)
'(use-package chronometer)
'(use-package lsp-haskell)
'(use-package lsp-java)
'(use-package lsp-latex)
'(use-package lsp-sonarlint)
(progn
  '(use-package flycheck
     :hook ((after-init-hook . global-flycheck-mode)))
  '(use-package flycheck-eldev)
  '(use-package flycheck-clj-kondo))

;; MARKDOWN
(progn
  '(use-package markdown-mode)
  '(use-package markdown-preview-mode))

'(use-package eldoc
   :diminish)

;; Travelling through CamelCase
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
;; Useful for Clojure since there are Java classes.
'(use-package subword
   :diminish)

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


;; For Clojure CLR
'(use-package inf-clojure)

'(use-package html-to-hiccup)


;; Programming languages
;; https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
'(progn
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

'(use-package jsdoc)
'(use-package markdown-soma)
'(use-package keycast)
'(use-package racket-mode)

'(use-package slime)
'(use-package php-mode)
'(use-package dockerfile-mode)
'(use-package ocaml-ts-mode)
'(use-package cargo-mode)
'(use-package rust-mode)
'(use-package zig-mode)

;; https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html





(when (version< emacs-version "28.2")
  (message "You should use the latest version of Emacs!"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Documentation
'(use-package helpful
   :bind
   ([remap describe-function] . helpful-function)
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command]  . helpful-command)
   ([remap describe-key]      . helpful-key))

'(use-package accent
   :custom
   (accent-position 'after)
   (accent-custom '((a (ă))
		    (o (ŏ))
		    (u (ŭ)))))

'(use-package ample-theme)
'(use-package apropospriate-theme)
'(use-package autumn-light-theme)
'(use-package chyla-theme)
'(use-package darcula-theme)
'(use-package flatland-theme)
'(use-package grey-paper-theme)
'(use-package gruber-darker-theme)
'(use-package gruvbox-theme)
'(use-package leuven-theme)
'(use-package material-theme)
'(use-package monokai-theme)
'(use-package seti-theme)
'(use-package spacegray-theme)
'(use-package sunburn-theme)
'(use-package tango-plus-theme)
'(use-package tangotango-theme)
'(use-package uwu-theme)
'(use-package zerodark-theme)

'(progn (load-theme 'ample t t)
	(load-theme 'ample-flat t t)
	(load-theme 'ample-light t t)
	(enable-theme 'ample-flat))

'(use-package dirvish
   :init
   (dirvish-override-dired-mode))

'(use-package ztree
   :init
   (push (substitute-in-file-name "path-to-ztree-directory") load-path))

'(use-package dir-treeview)

'(use-package better-scroll
   :init
   (define-key global-map (kbd "<prior>")   #'better-scroll-down)
   (define-key global-map (kbd "<next>")    #'better-scroll-up)
   (define-key global-map (kbd "S-<prior>") #'better-scroll-down-other-window)
   (define-key global-map (kbd "S-<next>")  #'better-scroll-up-other-window))

'(use-package units-mode
   :hook text-mode
   :config
   (local-set-key (kbd "C-c u") 'units-convert-region-and-insert))

;; savefile dir
'(progn ;; Est-ce que ça fonctionne ?
   (defconst custom-savefile-dir (expand-file-name "savefile" user-emacs-directory))
   (unless (file-exists-p custom-savefile-dir)
     (make-directory custom-savefile-dir))
   (setq backup-directory-alist
	 `((".*" . ,temporary-file-directory)))
   (setq auto-save-file-name-transforms
	 `((".*" ,temporary-file-directory t))))

'(use-package tabbar
   :init
   (tabbar-mode t))

;;(defvar ibuffer-expert t)

'(use-package perspective
   :bind (("C-x k"   . persp-kill-buffer*)
	  ("C-x C-b" . persp-list-buffers))
   :custom
   (persp-mode-prefix-key (kbd "C-c M-p"))
   :init (persp-mode))

;; A timer utility
'(use-package pomidor
   :bind (("C-M-<f12>" . pomidor))
   :config (setq pomidor-sound-tick nil
                 pomidor-sound-tack nil)
   :hook (pomidor-mode . (lambda ()
                           (display-line-numbers-mode -1) ; Emacs 26.1+
                           (setq left-fringe-width 0 right-fringe-width 0)
                           (setq left-margin-width 2 right-margin-width 0)
                           ;; force fringe update
                           (set-window-buffer nil (current-buffer)))))

;; Terminal
'(progn
   '(use-package equake
      :custom
      (equake-size-width 0.75)
      (equake-opacity-active 95)
      (equake-default-shell 'eshell)
      (equake-available-shells '("shell" "eshell"))
      :config
      (advice-add #'save-buffers-kill-terminal
		  :before-while #'equake-kill-emacs-advice))
   '(use-package better-shell)
   )

'(use-package company-quickhelp)
;;(use-package company-auctex)
'(use-package company-bibtex)
'(use-package company-cabal)
;;(use-package company-coq)
;;(use-package company-c-tags)
'(use-package company-ghci)
'(use-package company-go)
'(use-package company-manually)
'(use-package company-math)
'(use-package company-nginx)
'(use-package company-org-block)
'(use-package company-php)
'(use-package company-shell)
'(use-package company-wordfreq)
'(use-package flycheck-grammalecte)
'(use-package bash-completion)

'(use-package flycheck-haskell)

;; CRAP !!:
'(use-package clj-refactor
   :diminish)

'(use-package sotlisp
   :diminish)

'(use-package sotclojure
   :diminish)

;;(clojure-mode . clj-refactor-mode)
;;(clojure-mode . subword-mode)
;;(clojurescript-mode . clj-refactor-mode)
;;(clojurescript-mode . subword-mode)
;;(clojurec-mode . clj-refactor-mode)
;;(clojurec-mode . subword-mode)
;;(clojure-mode       . speed-of-thought-mode)
;;(clojurescript-mode . speed-of-thought-mode)
;;(clojurec-mode      . speed-of-thought-mode)

;;(show-paren-mode t)

;;(cider-jdk-src-paths '("c:/opt/jdk-19.0.1/lib/src.zip")) ;; is it used?
;;(cider-add-to-alist 'cider-jack-in-dependencies "mx.cider/enrich-classpath" "1.9.0")
;;(cider-add-to-alist 'cider-jack-in-dependencies "mx.cider/tools.deps.enrich-classpath" "1.9.0")
;;(cider-clojure-cli-command "pwsh") ;; useless: do NOT jack-in with Clojure CLI, just CONNECT to an existing REPL loaded with middleware. See my deps.edn file.
;; maybe uselful for Leiningen jack-in, not for CLI
;;(cider-use-overlays 0) ;; ?

'(use-package racket-mode)
'(use-package dr-racket-like-unicode)

;; Git
'(progn
   '(use-package magit
      :bind (("C-x g" . magit-status)))
   '(use-package magit-todos)
   '(use-package dired-git
      :hook
      (dired-mode-hook . dired-git-mode)))

'(use-package anaconda-mode) ;; Python IDE
'(use-package basic-c-compile)
'(use-package go-mode)
'(use-package haskell-mode
   :hook ((haskell-mode . subword-mode)
	  (haskell-mode . interactive-haskell-mode)
	  (haskell-mode . haskell-doc-mode)))
'(use-package rust-mode)

'(use-package org-preview-html)

;; My functions

'(defun insert-defn (x)
   "Insert a defn stub with X as the function name."
   (list
    (read-string "Enter fn name: "))
   (insert "(defn " x "
  \"\"
  []
  )")
   (backward-char 14))

'(defun insert-defun (x)
   "Insert a defun stub with X as the function name."
   (interactive
    (list
     (read-string "Enter fun name: ")))
   (insert "(defun " x " ()
  \"\"
  (interactive)
  )
  
(global-set-key (kbd \"\") '" x ")")
   (backward-char 57))

'(defun insert-defmacro (x)
   "Insert a defmacro stub with X as the macro name."
   (interactive
    (list
     (read-string "Enter macro name: ")))
   (insert "(defmacro " x "
  \"\"
  []
  `())")
   (backward-char 13))

'(use-package echo-bar)

;; numéroter les recherches
'(use-package anzu
   :init (anzu-mode t))

'(use-package avy
   :init
   (global-set-key (kbd "C-:")   #'avy-goto-char)
   (global-set-key (kbd "C-'")   #'avy-goto-char-2)
   (global-set-key (kbd "M-g f") #'avy-goto-line)
   (global-set-key (kbd "M-g w") #'avy-goto-word-1))

'(use-package avy-flycheck
   :init
   (global-set-key (kbd "C-c '") #'avy-flycheck-goto-error))

'(use-package blackboard-bold-mode)
'(use-package fraktur-mode)

'(use-package apache-mode)
;;(use-package auctex)

'(use-package line-reminder
   :diminish
   :init
   (global-line-reminder-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TO BE TESTED

'(server-start) ;; Does it work on Windows? Do I have to keep the first instance alive?

'(use-package arduino-mode)
'(use-package auto-dictionary)
'(use-package auto-dim-other-buffers)
'(use-package auto-highlight-symbol)
'(use-package auto-indent-mode)
'(use-package auto-org-md)
'(use-package auto-package-update)
'(use-package auto-rename-tag)
'(use-package auto-shell-command)
'(use-package babel)
'(use-package babel-repl)
'(use-package back-button)
'(use-package backline) ;; ?
'(use-package backup-each-save)
'(use-package backup-walker)
'(use-package bbdb) ;; Big Brother DB = ?
'(use-package better-defaults)
'(use-package better-jumper)
'(use-package bicycle)
'(use-package bind-chord)
'(use-package bison-mode)
'(use-package blist)
'(use-package smex :init (smex-initialize) :bind ("M-x" . smex))
'(add-to-list 'load-path (substitute-in-file-name "${THIS_DRIVE}/opt/elisp"))
'(require 'iy-go-to-char)
'(use-package key-chord
   :init
   (key-chord-mode t)
   (key-chord-define-global "hj" 'undo)
   (key-chord-define-global "fg" 'iy-go-to-char)
   (key-chord-define-global "df" 'iy-go-to-char-backward))
;;(key-chord-define-global ",." "<>\C-b")
'(use-package all-the-icons ;; télécharger les icônes préalablement
   :if (display-graphic-p)
   :load-path all-the-icons-p)
'(use-package all-the-icons-completion)
'(use-package all-the-icons-ibuffer
   :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
'(load "C:/opt/elisp/all-the-icons-dired.el")
'(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
'(use-package ivy
   :init (ivy-mode t))
'(use-package counsel
   :after ivy
   :bind (("M-x"     . counsel-M-x)
          ("C-x b"   . counsel-ibuffer)
          ("C-x C-f" . counsel-find-file)
          ("C-M-j"   . counsel-switch-buffer)
          ("C-M-l"   . counsel-imenu)
          :map minibuffer-local-map
          ("C-r"     . counsel-minibuffer-history))
   :custom (ivy-initial-inputs-alist nil)) ;; Don't start searches with ^
'(use-package ivy-rich
   :init (ivy-rich-mode t)
   :after counsel)
'(use-package all-the-icons-ivy
   :init (all-the-icons-ivy-setup))
'(use-package all-the-icons-ivy-rich
   :init (all-the-icons-ivy-rich-mode t))
'(use-package projectile) ;; Anything useful in there? 'projects' is built-in.
'(use-package company-fuzzy
   :hook (company-mode . company-fuzzy-mode)
   :custom
   (company-fuzzy-sorting-backend 'alphabetic)
   (company-fuzzy-prefix-on-top nil)
   (company-fuzzy-history-backends '(company-yasnippet))
   (company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
   :config
   (global-company-fuzzy-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RECIPES

;;(setq initial-major-mode 'fundamental-mode)
;;(kill-buffer "*scratch*")
;;(setq initial-buffer-choice user-init-file)

;; With dashboard, this is useless:
'(defun todev/emacs-startup-hook ()
   "Change directory to my development dir."
   (cd "c:/dev"))

;;(add-hook 'emacs-startup-hook #'todev/emacs-startup-hook)
;; To pin a package to a certain version:
;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

'(when nil ;; For demos only
   (use-package command-log-mode)
   (global-command-log-mode t)
   (clm/toggle-command-log-buffer))

'(
  (mapcar #'1+ '(1 2 3))
  )

;; emacs -q -l init.el

