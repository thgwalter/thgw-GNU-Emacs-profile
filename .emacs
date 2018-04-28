;;;;;;;;;;;;
;;; TEXT ;;;
;;;;;;;;;;;;

;; Use explicitly UTF-8 rather than the default OS encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;;;;;;;;;;
;;; UI ;;;
;;;;;;;;;;

;; A dark theme available out of the GNU Emacs box
(load-theme 'tsdh-dark)
;; After reading the Emacs tutorial
;; shortcuts usually replace the mousy options
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
;; Blinking the cursor is not useful
(blink-cursor-mode 0)
(show-paren-mode t)
;; Toggle the menus is convenient to learn new modes
(global-set-key [f5] 'toggle-menu-bar-mode-from-frame)
;; Font size at 12 pt
(set-face-attribute 'default nil :height 120)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PACKAGES AND HOOKS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; packages list
(dolist (p '(paredit aggressive-indent clojure-mode cider company))
  (unless (package-installed-p p) (package-install p)))
;; hooks
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook
	  '(lambda ()
	     (define-key cider-repl-mode-map
	       (kbd "C-c M-b")
	       'cider-repl-clear-buffer)))
(setq cider-repl-display-help-banner nil)


;;;;;;;;;;;;;;;;;;;;;;
;;; AUTOMATIC CODE ;;;
;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company cider clojure-mode aggressive-indent paredit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
