(require 'package)

;; ============================
;; Sources
;; ============================

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

;; ============================
;; Install packages
;; ============================
(defvar myPackages
  ' (better-defaults                 ;; Set up some better Emacs default
     flycheck                             ;; On the fly syntax checking in python
     py-autopep8                    ;; Run autopep8 on save
     ein                            ;; Emacs Ipython Notebook
     )
   )

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ===================================
;; Basic Customization
;; ===================================

;; Disable toolbar, menu-bar, scroll-bar.
(tool-bar-mode -1) 
(menu-bar-mode -1)
(toggle-scroll-bar -1) 

(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'solarized-dark t)        
(global-linum-mode t)               ;; Enable line numbers globally

;; disable creating backup files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files

;; Spell-check
(require 'flyspell)
(setq flyspell-issue-message-flag nil
      ispell-local-dictionary "en_GB"
      ispell-program-name "/usr/local/bin/aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; ====================================
;; Development Setup
;; ====================================

;; Org-Mode
(use-package org
  :ensure t
  :config
  (setq org-todo-keywords
     '((sequence "TODO" "PROGRESS" "|" "DONE" "DELEGATED" "CANCELLED")))

  ;; Hide markers like *bold* and /italic/. 
  (setq org-hide-emphasis-markers t)

  ;; Change lists created by '-' to '•''
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([+-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Use bullets instead of asterisks (the defaults)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  )

;; ====================================
;; Development Setup
;; ====================================

;; Evil-Mode
(use-package evil
  :ensure t
  :init
  ;; Enable C-u in normal mode (Up).
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)

  ;; Disable evil-mode for some modes
  ;; (evil-set-initial-state 'help-mode 'emacs)
  ;; (add-to-list 'evil-emacs-state-modes 'org-mode)
  
  ;; Enable hs-minor-mode to the relevant modes (for folding)
  (add-hook 'c-mode-common-hook #'hs-minor-mode)
  (add-hook 'html-mode-common-hook #'hs-minor-mode)
  (add-hook 'python-mode-hook #'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
  (add-hook 'csharp-mode-hook #'hs-minor-mode)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)

    ;; Setting the leader to space.
    (evil-leader/set-leader "<SPC>")

    ;; launching ranger
    (evil-leader/set-key "r" 'ranger)

    ;; -------------------------------------------
    ;; Search commands
    ;; -------------------------------------------

    ;; Search file in project 
    (evil-leader/set-key "sf" 'helm-projectile)
    ;; Search in files
    (evil-leader/set-key "sif" 'helm-projectile-grep)
    ;; Search in imenu
    (evil-leader/set-key "sii" 'helm-imenu)
    ;; Go to definition
    (evil-leader/set-key "sd" 'helm-cscope-find-global-definition-no-prompt)
    ;; Cross-references
    (evil-leader/set-key "sx" 'helm-cscope-find-this-symbol-no-prompt)
    ;; List all references inside current function
    (evil-leader/set-key "sX" 'helm-cscope-find-called-function-no-prompt)

    ;; -------------------------------------------
    ;; Projects commands
    ;; -------------------------------------------
    ;; Project switch
    (evil-leader/set-key "ps" 'helm-projectile-switch-project)


    ;; -------------------------------------------
    ;; Fold command (z in vim is for foldings)
    ;; -------------------------------------------
    
    ;; Fold all by level
    (evil-leader/set-key "zl" 'hs-hide-level)

    ;; -------------------------------------------
    ;; Git commands
    ;; -------------------------------------------
    
    ;; Git status
    (evil-leader/set-key "gs" 'magit-status)
    
    ;; -------------------------------------------
    ;; Toggle commands
    ;; -------------------------------------------

    ;; toggle imenu window
    (evil-leader/set-key "ti" 'imenu-list-smart-toggle)

    ;; toggle speedbar
    (evil-leader/set-key "ts" (lambda() (interactive) (sr-speedbar-toggle)))

    ;; toggle treemacs
    (evil-leader/set-key "tt" (lambda() (interactive) (treemacs)))

    ;; -------------------------------------------
    ;; Open commands
    ;; -------------------------------------------

    ;; open config file
    (evil-leader/set-key "oc" (lambda() (interactive) (find-file user-init-file)))

    ;; -------------------------------------------
    ;; Comments commands
    ;; -------------------------------------------

    ;; Comment line
    (evil-leader/set-key "cl" 'evilnc-comment-or-uncomment-lines)
    ;; Comment operator
    (evil-leader/set-key "co" 'evilnc-comment-operator)

    ;;;; -------------------------------------------
    ;; Buffers commands
    ;; -------------------------------------------

    ;; buffers before
    (evil-leader/set-key "bb" 'mode-line-other-buffer)
    ;; buffers switch
    (evil-leader/set-key "bs" 'helm-buffers-list)
    ;; buffers list
    (evil-leader/set-key "bl" 'buffer-menu)
    ;; buffer kill
    (evil-leader/set-key "bk" 'kill-this-buffer)

    ;; -------------------------------------------
    ;; Windows commands
    ;; -------------------------------------------

    ;; window kill
    (evil-leader/set-key "wk" 'delete-window)
    ;; window horizontal split
    (evil-leader/set-key "whs" 'split-window-below)
    ;; window vertical split
    (evil-leader/set-key "wvs" 'split-window-right)
    
    ;; -------------------------------------------
    ;; Frames commands
    ;; -------------------------------------------

    ;; frame create 
    (evil-leader/set-key "fc" 'make-frame-command)
    ;; frame kill
    (evil-leader/set-key "fk" 'delete-frame)

    ;; -------------------------------------------
    ;; Globals commands
    ;; -------------------------------------------

    ;; Kill buffer and windows
    (evil-leader/set-key "k" 'kill-buffer-and-window)
    ;; Update (save)
    (evil-leader/set-key "u" 'save-buffer)
    ;; Quit
    (evil-leader/set-key "q" 'kill-emacs)
  )

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-magit
    :ensure t)

  (use-package evil-indent-plus
    :ensure t
    :config
    ;; Adding indent-text-objects
    (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
    (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
    (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
    (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
    (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
    (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down)
  )

)

(use-package ein
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (add-hook 'ein:notebook-mode-hook (lambda () (setq-local before-save-hook nil)))
    (with-eval-after-load 'evil
      (evil-define-key 'normal ein:notebook-mode-map
        "J" 'ein:worksheet-goto-next-input
        "K" 'ein:worksheet-goto-prev-input
        "\\w" 'ein:notebook-save-notebook-command
        "b" 'ein:worksheet-insert-cell-below
        "\\B" (lambda ()
                (interactive)
                (setq current-prefix-arg t)
                (call-interactively 'ein:worksheet-insert-cell-below)
                )
        "\\a" 'ein:worksheet-insert-cell-above
        "\\A" (lambda ()
                (interactive)
                (setq current-prefix-arg t)
                (call-interactively 'ein:worksheet-insert-cell-above)
                )
        "\\d" 'ein:worksheet-delete-cell
        "\\c" 'ein:worksheet-clear-output
        "\\z" 'ein:notebook-kernel-interrupt-command
        "\\o" 'ein:console-open
        (concat "\\s" (kbd "RET")) (lambda ()
                                     (interactive)
                                     (run-python)
                                     (let* ((cell (ein:worksheet-get-current-cell :cell-p #'ein:codecell-p))
                                            (code (ein:cell-get-text cell))
                                           )
                                       (python-shell-send-string code)
                                       )
                                     )
        (kbd "<f5>") 'ein:worksheet-execute-all-cell
        (kbd "RET" ) 'ein:worksheet-execute-cell
        (kbd "SPC") 'ein:worksheet-execute-cell-and-goto-next
        )
      )
    )
  )

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
(require 'evil-magit)
(use-package evil-magit
  :ensure t
  :defer t)

;; Flycheck mode
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;; yasnippets
(use-package yasnippet
  :ensure t
  :config
  ;; Sinppets colletions for all the languages!!
  (use-package yasnippet-snippets
    :ensure t
    :config
    ;; enable yasnippet on specific modes
    (yas-reload-all)
    (add-hook 'c-mode-common-hook #'yas-minor-mode)
    (add-hook 'python-mode-hook #'yas-minor-mode)
    )
  )

;; Python auto-complete
;(use-package jedi
;  :ensure t
;  :config
;  (add-hook 'python-mode-hook 'jedi:setup)
;  (setq jedi:complete-on-dot t)
;  )

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
    (jedi yasnippet-snippets evil-magit ein py-autopep8 elpy material-theme better-defaults multi-term grep-a-lot solarized-theme use-package evil-visual-mark-mode)))
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



