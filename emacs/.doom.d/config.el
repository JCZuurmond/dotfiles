;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Prefer newer changes
(setq load-prefer-newer t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cor Zuurmond"
      user-mail-address "cor.zuurmond@xebia.com")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'solarized-dark)

;; Org
(after! org
  ;; org-subtree-archive failed with following error: org-id-add-location: Wrong type argument: hash-table-p, nil
  ;; solution: https://github.com/org-roam/org-roam/issues/1526#issuecomment-901663871
  (org-id-update-id-locations)
  (setq org-directory "~/Dropbox (Xebia)/org/")
  (setq org-startup-indented nil
        org-adapt-indentation nil
        org-hide-leading-stars nil
        visual-line-mode t)
  (setq org-default-notes-file (concat org-directory "2-notes.org"))
  (setq org-agenda-files
        '("~/Dropbox (Xebia)/org/0-todo.org"
          "~/Dropbox (Xebia)/org/1-base.org"
          "~/Dropbox (Xebia)/org/2-notes.org"
          "~/Dropbox (Xebia)/org/3-journal.org"
          "~/Dropbox (Xebia)/org/4-companies.org"))
  (setq org-mobile-directory "~/Dropbox (Xebia)/Apps/MobileOrg")
  (setq org-mobile-inbox-for-pull (concat org-directory "from-mobile-new.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROG(p!)" "|" "DONE(d!)" "DONT(x@/!)"))))

(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(add-hook 'yaml-mode-hook 'disable-tabs)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default enable-local-variables t)

;; Fix highlights in text mode
(setq-hook! 'text-mode-hook indent-tabs-mode t)

(defcustom python-projectile-environment-directory ".direnv/python-3.7.2"
  "The python environment within a projectile project"
  :type 'string
  :group 'python)

;; Run a hook after local vars are read
;; Source: https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
(defun run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)

;; Set-up the python shell
(defun config/python-mode-shell-setup ()
  (message "project python environment is %s" python-projectile-environment-directory)
  (setq-local python-shell-virtualenv-root (expand-file-name python-projectile-environment-directory (projectile-project-root))
              python-pytest-executable (expand-file-name (concat python-projectile-environment-directory "/bin/pytest -x -s --pdbcls=IPython.core.debugger:Pdb") (projectile-project-root))
              lsp-python-ms-python-executable (expand-file-name (concat python-projectile-environment-directory "/bin/python") (projectile-project-root))
              lsp-pyright-venv-path (expand-file-name python-projectile-environment-directory (projectile-project-root))
              lsp-pyright-venv-directory python-projectile-environment-directory))

(add-hook 'python-mode-local-vars-hook 'config/python-mode-shell-setup)

(setq lsp-pylsp-plugins-flake8-ignore ["D101"])

(setq-hook! 'python-mode-hook +format-with 'black)

;; OpenAI
(defun openai-get-key ()
  "Retrieve the OpenAI API key from Azure CLI."
  (let* ((output (with-output-to-string
                  (call-process "az" nil standard-output nil
                                  "account" "get-access-token" "--resource" "https://cognitiveservices.azure.com")))
          (json (json-read-from-string output)))
  (cdr (assoc 'accessToken json))))

(after! openai
  (setq openai-key #'openai-get-key
        openai-base-url "https://slackgpt-openai.openai.azure.com/openai/deployments/gpt-35-turbo"
        openai-completion-max-tokens 1000
        openai-parameters '(("api-version" . "2023-03-15-preview"))))

;; Codegpt
(after! codegpt
  (setq codegpt-tunnel 'chat
       codegpt-model "gpt-3.5-turbo"))

;; Overwrite the evil-escape-key-sequence
;; TODO: Document why
(use-package-hook! evil-escape
  :post-init
  (setq evil-escape-key-sequence "qx")
  t)
