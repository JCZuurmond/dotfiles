;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; ============================================================================
;;; CORE CONFIGURATION
;;; ============================================================================

;; Prefer newer changes
(setq load-prefer-newer t)

;; User identification (from git config)
(setq user-full-name (string-trim (shell-command-to-string "git config user.name"))
      user-mail-address (string-trim (shell-command-to-string "git config user.email")))

;; UI settings
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq doom-theme 'solarized-dark
      display-line-numbers-type t)

;; Enable local variables and text mode tab handling
(setq-default enable-local-variables t)
(setq-hook! 'text-mode-hook indent-tabs-mode t)

;; Run hook for major-mode after local variables are processed
(defun run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)

;;; ============================================================================
;;; DEVELOPMENT TOOLS
;;; ============================================================================

;; Tab handling
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(add-hook 'yaml-mode-hook 'disable-tabs)

;; Python development with auto virtual environment detection
(after! python-mode
  (defun config/python-mode-shell-setup ()
    "Configure Python environment, auto-detecting from $VIRTUAL_ENV (direnv) when available."
    (when-let ((venv-path (or (getenv "VIRTUAL_ENV")
                              (when (projectile-project-root)
                                (let ((direnv-venv (expand-file-name ".venv" (projectile-project-root))))
                                  (when (file-directory-p direnv-venv) direnv-venv))))))
      (message "Python environment: %s" venv-path)
      (setq-local python-shell-virtualenv-root venv-path
                  python-pytest-executable (concat venv-path "/bin/pytest")
                  lsp-python-ms-python-executable (concat venv-path "/bin/python")
                  lsp-pyright-venv-path (file-name-directory venv-path)
                  lsp-pyright-venv-directory (file-name-nondirectory venv-path))))

  (add-hook 'python-mode-local-vars-hook 'config/python-mode-shell-setup)

  ;; Python tool configuration
  (setq python-pytest-arguments '("-x" "-s" "--pdbcls=IPython.core.debugger:Pdb")
        lsp-pylsp-plugins-flake8-ignore ["D101"]
        +format-with 'black))

;;; ============================================================================
;;; AI/LLM CONFIGURATION
;;; ============================================================================

;; Azure OpenAI integration
(after! openai
  (defun openai-get-key ()
    "Retrieve the OpenAI API key from Azure CLI."
    (let* ((output (with-output-to-string
                    (call-process "az" nil standard-output nil
                                  "account" "get-access-token" "--resource" "https://cognitiveservices.azure.com")))
           (json (json-read-from-string output)))
      (cdr (assoc 'accessToken json))))

  (setq openai-key #'openai-get-key
        openai-base-url "https://slackgpt-openai.openai.azure.com/openai/deployments/gpt-35-turbo"
        openai-completion-max-tokens 1000
        openai-parameters '(("api-version" . "2023-03-15-preview"))))

;; GitHub Copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))

;; gptel - Primary LLM client with agentic capabilities
(use-package! gptel
  :config
  (require 'gptel-integrations)
  (require 'gptel-org)

  ;; Core gptel configuration
  (setq gptel-model 'claude-sonnet-4
        gptel-default-mode 'org-mode
        gptel-use-curl t
        gptel-use-tools t
        gptel-confirm-tool-calls 'always
        gptel-include-tool-results 'auto
        gptel-stream t
        gptel-backend (gptel-make-gh-copilot "Copilot" :stream t)))

;; gptel-agent for agentic capabilities
(use-package! gptel-agent
  :after gptel
  :config
  (gptel-agent-update))

;; Claude Code IDE integration
(use-package! claude-code-ide
  :commands (claude-code-ide claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'vterm
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 90)
  (claude-code-ide-emacs-tools-setup))

;; MCP (Model Context Protocol) integration
(use-package! mcp
  :after gptel
  :custom
  (mcp-hub-servers
   `(("duckduckgo" . (:command "uvx" :args ("duckduckgo-mcp-server")))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("filesystem" . (:command "npx"
                      :args ("-y" "@modelcontextprotocol/server-filesystem" ,(getenv "HOME"))
                      :roots (,(getenv "HOME"))))
     ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
     ("github" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-github")))
     ("memory" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-memory")))))
  :config
  (require 'mcp-hub)
  (mcp-hub-start-all-server))

;; Gemini CLI integration (OAuth-based, no API key needed)
(use-package! gemini-cli
  :commands (gemini-cli gemini-cli-send-region gemini-cli-fix-error-at-point)
  :config
  (setq gemini-cli-program "gemini"
        gemini-cli-terminal-backend 'vterm))

;; Helper functions for AI tools
(defun my/gptel-context-list ()
  "Display current gptel contexts."
  (interactive)
  (let ((output ""))
    (if gptel-context--alist
        (progn
          (setq output (format "Found %d contexts:\n" (length gptel-context--alist)))
          (dolist (context gptel-context--alist)
            (let* ((buffer (car context))
                   (overlays (plist-get (cdr context) :overlays))
                   (buffer-name (buffer-name buffer)))
              (setq output (concat output (format "Buffer: %s\n" buffer-name)))
              (dolist (overlay overlays)
                (setq output (concat output (format "  Region: %d-%d (%d chars)\n"
                                                   (overlay-start overlay)
                                                   (overlay-end overlay)
                                                   (- (overlay-end overlay) (overlay-start overlay)))))))))
      (setq output "No gptel contexts found."))
    (message "%s" output)
    output))

;;; ============================================================================
;;; ZETTELKASTEN (MARKDOWN-BASED NOTE SYSTEM)
;;; ============================================================================

;; Vault configuration
(defvar zettelkasten-directory "~/org/"
  "Root directory for the Markdown Zettelkasten vault.")

;; Ensure directory exists
(unless (file-exists-p zettelkasten-directory)
  (make-directory zettelkasten-directory t))

;; md-roam for Obsidian compatibility
(use-package! md-roam
  :after org-roam
  :config
  (md-roam-mode 1)
  (setq md-roam-file-extension "md"
        md-roam-use-title-for-link-descr t))

;; org-roam configuration for Zettelkasten
(after! org-roam
  ;; Core settings
  (setq org-roam-directory (file-truename zettelkasten-directory)
        org-roam-file-extensions '("md")
        org-roam-db-location (concat zettelkasten-directory ".org-roam.db"))

  ;; Filename generation
  (defun zettelkasten-slug (title)
    "Generate a timestamped filename slug from TITLE."
    (let ((slug (downcase (replace-regexp-in-string
                           "[^a-zA-Z0-9]+" "-"
                           (replace-regexp-in-string "^-\\|-$" "" title)))))
      (format-time-string (concat "%Y%m%d%H%M-" slug))))

  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Generate a timestamped slug for NODE."
    (zettelkasten-slug (org-roam-node-title node)))

  ;; Capture templates for different note types
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: []\n---\n\n# ${title}\n\n")
           :unnarrowed t)
          ("n" "note" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [note]\n---\n\n# ${title}\n\n")
           :unnarrowed t)
          ("l" "literature" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [literature]\nsource: \nauthor: \n---\n\n# ${title}\n\n## Summary\n\n## Key Ideas\n\n## References\n\n")
           :unnarrowed t)
          ("p" "permanent" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [permanent]\n---\n\n# ${title}\n\n## Idea\n\n## Context\n\n## Connections\n\n")
           :unnarrowed t)))

  ;; Daily notes
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry "## %<%H:%M>\n%?"
           :target (file+head "%<%Y-%m-%d>.md"
                              "---\ntitle: \"%<%Y-%m-%d>\"\ndate: %<%Y-%m-%d>\ntags: [daily]\n---\n\n# %<%A, %B %d, %Y>\n\n## Tasks\n\n- [ ] \n\n## Notes\n\n")))))

;; Inbox workflow for task management
(defvar zettelkasten-inbox-file (concat zettelkasten-directory "inbox.md")
  "Path to the central inbox file.")

(defvar zettelkasten-archive-file (concat zettelkasten-directory "archive.md")
  "Path to the archive file.")

(defvar zettelkasten-archive-directory (concat zettelkasten-directory "archive/")
  "Path to the archive directory.")

;; Inbox functions
(defun zettelkasten-open-inbox ()
  "Open the Zettelkasten inbox file."
  (interactive)
  (find-file zettelkasten-inbox-file))

(defun zettelkasten-capture-to-inbox ()
  "Quickly capture a task to the inbox file."
  (interactive)
  (let ((task (read-string "Task: ")))
    (with-current-buffer (find-file-noselect zettelkasten-inbox-file)
      (goto-char (point-min))
      (if (re-search-forward "^## Tasks" nil t)
          (progn
            (forward-line 1)
            (end-of-line)
            (insert (format "\n- [ ] %s" task)))
        (goto-char (point-max))
        (insert (format "\n- [ ] %s" task)))
      (save-buffer))
    (message "Task captured: %s" task)))

(defun zettelkasten-toggle-checkbox ()
  "Toggle markdown checkbox at point between [ ] and [x]."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "\\(.*\\)- \\[ \\]")
      (replace-match "\\1- [x]"))
     ((looking-at "\\(.*\\)- \\[x\\]")
      (replace-match "\\1- [ ]")))))

(defun zettelkasten-review-tasks ()
  "Search for all open markdown checkboxes across the vault."
  (interactive)
  (let ((default-directory zettelkasten-directory))
    (consult-ripgrep zettelkasten-directory "- \\[ \\]")))

;; Wikilink support for Obsidian compatibility
(defun zettelkasten-insert-wikilink ()
  "Insert a title-based wikilink for Obsidian compatibility."
  (interactive)
  (let* ((node (org-roam-node-read))
         (title (org-roam-node-title node)))
    (insert (format "[[%s]]" title))))

;; Markdown-mode enhancements
(after! markdown-mode
  (setq markdown-enable-wiki-links t
        markdown-wiki-link-search-type '(project))

  (map! :map markdown-mode-map
        :n "gf" #'zettelkasten-follow-wikilink-at-point
        :n "RET" #'zettelkasten-follow-wikilink-at-point))

;;; ============================================================================
;;; ORG-MODE (MINIMAL CONFIGURATION)
;;; ============================================================================

(after! org
  ;; Fix for org-id issue with org-roam
  (org-id-update-id-locations)

  ;; Basic settings
  (setq org-startup-indented nil
        org-adapt-indentation nil
        org-hide-leading-stars nil
        visual-line-mode t
        org-agenda-files (list (concat org-directory "inbox.org"))
        org-archive-location (concat org-directory "archive.org::datetree/")
        org-agenda-start-with-log-mode t
        org-log-into-drawer t
        org-log-done 'time
        org-todo-keywords '((sequence "TODO(t)" "PROG(p!)" "|" "DONE(d!)" "DONT(x@/!)"))))

;;; ============================================================================
;;; KEYBINDINGS
;;; ============================================================================

;; Evil escape configuration
(use-package-hook! evil-escape
  :post-init
  (setq evil-escape-key-sequence "qx")
  t)

;; Set local leader
(setq doom-localleader-key ",")

;; LLM/AI keybindings
(map! :leader
      (:prefix ("l" . "LLM")
       :desc "Send to LLM" "s" #'gptel-send
       :desc "Open agentic chat" "c" #'gptel-agent
       :desc "Claude Code IDE" "C" #'claude-code-ide
       :desc "Open chat" "A" #'gptel
       :desc "Menu" "m" #'gptel-menu
       :desc "Tools menu" "t" #'gptel-tools
       :desc "Rewrite region" "r" #'gptel-rewrite
       :desc "Add context" "a" #'gptel-add
       :desc "Add file" "f" #'gptel-add-file
       :desc "Gemini" "g" #'gemini-cli
       :desc "MCP hub" "h" #'mcp-hub
       :desc "Restart MCP" "R" #'mcp-hub-restart-all-server))

;; Zettelkasten keybindings
(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("r" . "roam")
        :desc "Find node" "f" #'org-roam-node-find
        :desc "Insert link" "i" #'zettelkasten-insert-wikilink
        :desc "Capture note" "c" #'org-roam-capture
        :desc "Toggle buffer" "b" #'org-roam-buffer-toggle
        :desc "Dailies today" "t" #'org-roam-dailies-goto-today
        :desc "Open inbox" "I" #'zettelkasten-open-inbox
        :desc "Quick task" "q" #'zettelkasten-capture-to-inbox
        :desc "Toggle checkbox" "x" #'zettelkasten-toggle-checkbox
        :desc "Review tasks" "R" #'zettelkasten-review-tasks
        :desc "Sync database" "s" #'org-roam-db-sync)))
