;; ============================
;; MELPA Package Support
;; ============================
(require 'package)

(add-to-list 'package-archives
	            '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	           '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	           '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set-up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq package-check-signature 'allow-unsigned)
(add-to-list 'package-unsigned-archives "undo-tree-0.6.3")

;; ============================
;; Install packages
;; ============================
(defvar myPackages
  ' (better-defaults                 ;; Set up some better Emacs default
     flycheck                             ;; On the fly syntax checking in python
     py-autopep8                    ;; Run autopep8 on save
     ein                            ;; Emacs Ipython Notebook
     solarized-theme                ;; color scheme 
;;     evil-magit                     ;; git
	 grep-a-lot
	 evil
     )
   )

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ===================================
;; Basic Customization
;; ===================================

(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'solarized-dark t)        
(global-linum-mode t)               ;; Enable line numbers globally

;; ====================================
;; Development Setup
;; ====================================
(setq python-shell-interpreter "python3")

;; Enable elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3"))

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))

;; Magit
(use-package magit
  :ensure t
  :defer t
  :config
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

;; Evil-magit
;;(require 'evil-magit)
;;(use-package evil-magit
;;  :ensure t
;;  :defer t)

;; Flycheck mode
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;; ============================
;; Shell
;; ============================
; Run shell in interactive mode
(setq shell-command-switch "-ic")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

(require 'evil)
(evil-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(explicit-shell-file-name nil)
 '(hl-sexp-background-color "#1c1f26")
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("/tmp/test.org")))
 '(package-selected-packages
   (quote
    (evil-magit ein py-autopep8 elpy material-theme better-defaults multi-term grep-a-lot solarized-theme use-package evil-visual-mark-mode)))
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(server-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Hack")))))



(setq org-todo-keywords
      '((sequence "TODO" "PROGRESS" "|" "DONE" "DELEGATED" "CANCELLED")))
