;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Prefer newer changes
(setq load-prefer-newer t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. Loaded from git config.
(setq user-full-name (string-trim (shell-command-to-string "git config user.name"))
      user-mail-address (string-trim (shell-command-to-string "git config user.email")))

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

  ;; Org directory (ProtonDrive synced)
  (setq org-directory "~/org/")

  ;; Display settings
  (setq org-startup-indented nil
        org-adapt-indentation nil
        org-hide-leading-stars nil
        visual-line-mode t)

  ;; Default notes file
  (setq org-default-notes-file (concat org-directory "inbox.org"))

  ;; Agenda files - inbox is the main task file
  (setq org-agenda-files (list (concat org-directory "inbox.org")))

  ;; Archive location (datetree by year)
  (setq org-archive-location (concat org-directory "archive.org::datetree/"))

  ;; Agenda settings
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)

  ;; Todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROG(p!)" "|" "DONE(d!)" "DONT(x@/!)")))

  ;; Capture templates
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
           "* TODO %?\n%U\n" :prepend t)
          ("n" "Note" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %?\n%U\n" :prepend t)))

  ;; Refile targets
  (setq org-refile-targets `((org-agenda-files :maxlevel . 2)
                             (,(concat org-directory "notes.org") :maxlevel . 2)
                             (,(concat org-directory "inbox.org") :maxlevel . 2))))

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


;; ellama
;; https://github.com/s-kostyaev/ellama
(after! ellama
  (setq codegpt-tunnel 'chat
       codegpt-model "gpt-3.5-turbo"))


;; Overwrite the evil-escape-key-sequence
;; TODO: Document why
(use-package-hook! evil-escape
  :post-init
  (setq evil-escape-key-sequence "qx")
  t)

;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; Doom Defaults: `SPC' leader key, `SPC m' local leader
;; Practicalli: Set local leader to `,'
(setq doom-localleader-key ",")

;; Claude Code IDE
(use-package! claude-code-ide
  :commands (claude-code-ide claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'vterm
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 90)
  ;; Enable Emacs MCP tools (LSP, treesitter, imenu, etc.)
  (claude-code-ide-emacs-tools-setup))

;; Keybinding: SPC o c for "open claude"
(map! :leader
      :desc "Claude Code" "o c" #'claude-code-ide
      :desc "Claude Menu" "o C" #'claude-code-ide-menu)

;; ============================================================================
;; Settings migrated from old .emacs.d and .spacemacs configs (commented out)
;; Uncomment if you find yourself missing these features
;; ============================================================================

;; -- Magit fullscreen (from .spacemacs) --
;; Opens magit status in a fullframe window
;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1)

;; -- Org-mode settings (from .spacemacs) --
;; Prevents org from splitting lines when pressing M-RET
;; (setq org-M-RET-may-split-line nil)

;; -- Text mode auto-fill (from .spacemacs) --
;; Automatically wrap text at fill-column in text modes
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; -- Custom snippet directory (from .spacemacs) --
;; Add your custom snippets from ~/dotfiles/snippets/emacs/
;; (after! yasnippet
;;   (add-to-list 'yas-snippet-dirs "~/dotfiles/snippets/emacs/"))

;; -- Flyspell with aspell (from .emacs.d/init.el) --
;; Use aspell with British English dictionary
;; (after! ispell
;;   (setq ispell-program-name "/usr/local/bin/aspell"
;;         ispell-dictionary "en_GB"
;;         ispell-extra-args '("--sug-mode=ultra")))

;; -- Font settings (from .emacs.d/init.el) --
;; Hack font at size 12
;; (setq doom-font (font-spec :family "Hack" :size 12))

;; -- Language modes (from .spacemacs) --
;; You had csv, sql, html, json enabled in Spacemacs.
;; To enable in Doom, uncomment these in ~/.doom.d/init.el under :lang section:
;;   - web (for html)
;;   - json
;; And add under :lang:
;;   - data (includes csv, xml, etc.)
;;   - (sql +lsp)
