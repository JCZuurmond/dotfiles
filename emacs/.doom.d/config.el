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

;; Org - minimal config (notes moved to Markdown Zettelkasten below)
(after! org
  ;; Fix for org-id issue with org-roam
  ;; https://github.com/org-roam/org-roam/issues/1526#issuecomment-901663871
  (org-id-update-id-locations)

  ;; Display settings
  (setq org-startup-indented nil
        org-adapt-indentation nil
        org-hide-leading-stars nil
        visual-line-mode t)

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
        `(("t" "Todo" plain (file+function ,(concat org-directory "inbox.md") org-capture-goto-inbox-section)
           "- [ ] %?\n\n" :prepend nil)
          ("n" "Note" plain (file org-capture-note-file)
           ,(concat "---\n"
                    "title: %(org-capture-get-note-title)\n"
                    "date: %<%Y-%m-%d>\n"
                    "author: " user-full-name "\n"
                    "project: %(or (projectile-project-name) \"none\")\n"
                    "tags: []\n"
                    "---\n\n"
                    "# %(org-capture-get-note-title)\n\n"
                    "%?")
           :prepend nil)))

  ;; Generate note filename with YYYYMMDD-<title>.md format
  (defun org-capture-note-file ()
    "Generate a filename for a new note in notes/ subfolder."
    (let* ((title (read-string "Note title: "))
           (slug (org-capture-sanitize-filename title))
           (date (format-time-string "%Y%m%d"))
           (filename (concat org-directory "notes/" date "-" slug ".md")))
      ;; Store title for use in template
      (setq org-capture-note-title title)
      ;; Ensure notes directory exists
      (make-directory (concat org-directory "notes/") t)
      filename))

  ;; Convert title to filename-safe slug
  (defun org-capture-sanitize-filename (title)
    "Convert TITLE to a filename-safe slug."
    (let ((slug (downcase title)))
      (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
      (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
      slug))

  ;; Retrieve stored note title for use in template
  (defun org-capture-get-note-title ()
    "Return the stored note title for capture template."
    (or org-capture-note-title "Untitled"))

  ;; Function to find the "# Inbox" section in markdown files
  (defun org-capture-goto-inbox-section ()
    "Go to the # Inbox section for capture."
    (goto-char (point-min))
    (if (re-search-forward "^## Inbox" nil t)
        (progn
          (forward-line 1)
          ;; Skip any existing list items to append at the end
          (while (and (not (eobp))
                      (looking-at "^\\(- \\|$\\)"))
            (forward-line 1))
          ;; Go back one line if we're at a blank line after list items
          (when (and (not (bobp))
                     (save-excursion (forward-line -1) (looking-at "^$")))
            (forward-line -1)))
      (error "Could not find '# Inbox' section in inbox.md")))

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

;; Run a hook after local vars are read
;; Source: https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
(defun run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)

;; Set-up the python shell with auto-detection from direnv
(defun config/python-mode-shell-setup ()
  "Configure Python environment, auto-detecting from $VIRTUAL_ENV (direnv) when available."
  (let ((venv-path (or (getenv "VIRTUAL_ENV")
                       (when (projectile-project-root)
                         (let ((direnv-venv (expand-file-name ".venv" (projectile-project-root))))
                           (when (file-directory-p direnv-venv) direnv-venv))))))
    (when venv-path
      (message "Python environment: %s" venv-path)
      (setq-local python-shell-virtualenv-root venv-path
                  python-pytest-executable (concat venv-path "/bin/pytest")
                  lsp-python-ms-python-executable (concat venv-path "/bin/python")
                  lsp-pyright-venv-path (file-name-directory venv-path)
                  lsp-pyright-venv-directory (file-name-nondirectory venv-path)))))

(add-hook 'python-mode-local-vars-hook 'config/python-mode-shell-setup)

;; Pytest settings
(setq python-pytest-arguments '("-x" "-s" "--pdbcls=IPython.core.debugger:Pdb"))

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

;; gptel - LLM client with GitHub Copilot and agentic capabilities
(use-package! gptel
  :config
  (require 'gptel-integrations)
  (require 'gptel-org)

  ;; Set Copilot as the default backend with Claude model
  (setq gptel-model 'claude-sonnet-4
        gptel-default-mode 'org-mode
        gptel-use-curl t
        gptel-use-tools t
        gptel-confirm-tool-calls 'always
        gptel-include-tool-results 'auto
        gptel-stream t
        gptel-backend (gptel-make-gh-copilot "Copilot" :stream t)))

;; gptel-agent - Agentic capabilities
(use-package! gptel-agent
  :after gptel
  :config
  (gptel-agent-update))

;; MCP integration for agentic tool support
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
     ;; Additional MCP servers for enhanced capabilities
     ("git" . (:command "uvx" :args ("mcp-server-git" "--repository" ,(getenv "HOME"))))
     ("github" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-github")))
     ("memory" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-memory")))))
  :config
  (require 'mcp-hub)
  (mcp-hub-start-all-server))

;; Enhanced LLM/Agent keybindings with Claude integration
(map! :leader
      (:prefix ("l" . "LLM")

       :desc "Send to LLM" "s" #'gptel-send
       :desc "Open agentic chat" "c" #'gptel-agent
       :desc "Claude Code IDE" "C" #'claude-code-ide
       :desc "Open chat" "A" #'gptel
       :desc "Menu" "m" #'gptel-menu
       :desc "Claude Menu" "M" #'claude-code-ide-menu
       :desc "Tools menu" "t" #'gptel-tools
       :desc "Rewrite region" "r" #'gptel-rewrite
       :desc "Add context" "a" #'gptel-add
       :desc "Add file" "f" #'gptel-add-file
       :desc "GitHub login" "g" #'gptel-gh-login
       :desc "MCP hub" "h" #'mcp-hub
       :desc "Restart MCP servers" "R" #'mcp-hub-restart-all-server))


;; Helper functions
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

;; ============================================================================
;; Markdown Zettelkasten with Obsidian Compatibility (md-roam + org-roam)
;; Primary note-taking system - replaces org-mode notes
;; ============================================================================

;; Vault directory (ProtonDrive synced - same location as previous org setup)
(defvar zettelkasten-directory "~/org/"
  "Root directory for the Markdown Zettelkasten vault.")

;; Ensure the directory exists
(unless (file-exists-p zettelkasten-directory)
  (make-directory zettelkasten-directory t))

;; md-roam configuration (must load before org-roam)
(use-package! md-roam
  :after org-roam
  :config
  (md-roam-mode 1)
  ;; Use title-based wikilinks for Obsidian compatibility
  (setq md-roam-file-extension "md"
        md-roam-use-title-for-link-descr t))

;; org-roam configuration for Markdown Zettelkasten
(after! org-roam
  ;; Core settings
  (setq org-roam-directory (file-truename zettelkasten-directory)
        org-roam-file-extensions '("md")
        org-roam-db-location (concat zettelkasten-directory ".org-roam.db"))

  ;; Timestamp-based filename function
  (defun zettelkasten-slug (title)
    "Generate a timestamped filename slug from TITLE.
Format: YYYYMMDDHHMM-slugified-title.md"
    (let ((slug (downcase (replace-regexp-in-string
                           "[^a-zA-Z0-9]+" "-"
                           (replace-regexp-in-string "^-\\|-$" "" title)))))
      (format-time-string (concat "%Y%m%d%H%M-" slug))))

  ;; Capture templates for Obsidian-compatible Markdown notes
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: []\n---\n\n# ${title}\n\n")
           :unnarrowed t)

          ("t" "todo" plain "- [ ] %?"
           :target (file+head "inbox.md"
                              "---\ntitle: \"Inbox\"\ndate: %<%Y-%m-%d>\ntags: [inbox, tasks]\n---\n\n# Inbox\n\n## Tasks\n\n")
           :unnarrowed t)

          ("n" "note" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [note]\n---\n\n# ${title}\n\n")
           :unnarrowed t)

          ("f" "fleeting" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [fleeting]\n---\n\n# ${title}\n\n")
           :unnarrowed t)

          ("l" "literature" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [literature]\nsource: \nauthor: \n---\n\n# ${title}\n\n## Summary\n\n## Key Ideas\n\n## Quotes\n\n## References\n\n")
           :unnarrowed t)

          ("p" "permanent" plain "%?"
           :target (file+head "${slug}.md"
                              "---\ntitle: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [permanent]\n---\n\n# ${title}\n\n## Idea\n\n## Context\n\n## Connections\n\n")
           :unnarrowed t)))

  ;; Override the default slug function for timestamped filenames
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Generate a timestamped slug for NODE."
    (let ((title (org-roam-node-title node)))
      (zettelkasten-slug title)))

  ;; Configure link insertion to use title-based wikilinks
  (setq org-roam-extract-new-file-path "${slug}.md")

  ;; Daily notes template (for quick captures)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "## %<%H:%M>\n%?"
           :target (file+head "%<%Y-%m-%d>.md"
                              "---\ntitle: \"%<%Y-%m-%d>\"\ndate: %<%Y-%m-%d>\ntags: [daily]\n---\n\n# %<%A, %B %d, %Y>\n\n## Tasks\n\n- [ ] \n\n## Notes\n\n")))))

;; Function to insert Obsidian-compatible wikilinks
(defun zettelkasten-insert-wikilink ()
  "Insert a title-based wikilink [[Note Title]] for Obsidian compatibility."
  (interactive)
  (let* ((node (org-roam-node-read))
         (title (org-roam-node-title node)))
    (insert (format "[[%s]]" title))))

;; Function to create wikilinks from selected text
(defun zettelkasten-link-region-or-insert ()
  "If region is active, create a wikilink from it. Otherwise, prompt for a note."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[[%s]]" text)))
    (zettelkasten-insert-wikilink)))

;; ============================================================================
;; Inbox Workflow for Task Management
;; ============================================================================

(defvar zettelkasten-inbox-file (concat zettelkasten-directory "inbox.md")
  "Path to the central inbox file for task capture.")

(defvar zettelkasten-archive-file (concat zettelkasten-directory "archive.md")
  "Path to the archive file for completed tasks.")

;; Ensure inbox file exists with proper YAML header
(unless (file-exists-p zettelkasten-inbox-file)
  (with-temp-file zettelkasten-inbox-file
    (insert (format "---\ntitle: \"Inbox\"\ndate: %s\ntags: [inbox, tasks]\n---\n\n# Inbox\n\nCapture fleeting thoughts and tasks here.\n\n## Tasks\n\n- [ ] \n\n## Quick Notes\n\n"
                    (format-time-string "%Y-%m-%d")))))

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

(defun zettelkasten-capture-note-to-inbox ()
  "Quickly capture a note to the inbox file."
  (interactive)
  (let ((note (read-string "Note: ")))
    (with-current-buffer (find-file-noselect zettelkasten-inbox-file)
      (goto-char (point-min))
      (if (re-search-forward "^## Quick Notes" nil t)
          (progn
            (forward-line 1)
            (end-of-line)
            (insert (format "\n- %s (%s)" note (format-time-string "%Y-%m-%d %H:%M"))))
        (goto-char (point-max))
        (insert (format "\n- %s (%s)" note (format-time-string "%Y-%m-%d %H:%M"))))
      (save-buffer))
    (message "Note captured: %s" note)))

(defun zettelkasten-archive-done-task ()
  "Archive the completed task at point to archive.md with datetree structure."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "^- \\[x\\] \\(.*\\)$" line)
      (let* ((task (match-string 1 line))
             (year (format-time-string "%Y"))
             (month (format-time-string "%B"))
             (day (format-time-string "%d")))
        ;; Delete the line from current buffer
        (delete-region (line-beginning-position) (1+ (line-end-position)))
        (save-buffer)
        ;; Add to archive with datetree
        (with-current-buffer (find-file-noselect zettelkasten-archive-file)
          (goto-char (point-min))
          ;; Ensure YAML header exists
          (unless (looking-at "^---")
            (insert "---\ntitle: \"Archive\"\ndate: " (format-time-string "%Y-%m-%d") "\ntags: [archive]\n---\n\n# Archive\n\n"))
          ;; Find or create year heading
          (unless (re-search-forward (format "^## %s$" year) nil t)
            (goto-char (point-max))
            (insert (format "\n## %s\n" year)))
          ;; Find or create month heading under year
          (unless (re-search-forward (format "^### %s$" month) nil t)
            (end-of-line)
            (insert (format "\n\n### %s\n" month)))
          ;; Find or create day heading under month
          (unless (re-search-forward (format "^#### %s$" day) nil t)
            (end-of-line)
            (insert (format "\n\n#### %s\n" day)))
          ;; Add the task
          (end-of-line)
          (insert (format "\n- [x] %s" task))
          (save-buffer))
        (message "Archived: %s" task)))))

;; ============================================================================
;; Task Review: Aggregate all open tasks from the vault
;; ============================================================================

(defun zettelkasten-review-tasks ()
  "Search for all open markdown checkboxes across the Zettelkasten vault.
Uses consult-ripgrep to find and display all `- [ ]` items."
  (interactive)
  (let ((default-directory zettelkasten-directory))
    (consult-ripgrep zettelkasten-directory "- \\[ \\]")))

(defun zettelkasten-tasks-buffer ()
  "Create a buffer with all open tasks from the Zettelkasten vault."
  (interactive)
  (let* ((default-directory zettelkasten-directory)
         (buffer-name "*Zettelkasten Tasks*")
         (results (shell-command-to-string
                   (format "rg --no-heading --line-number '- \\[ \\]' %s"
                           (shell-quote-argument zettelkasten-directory)))))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "# Open Tasks in Zettelkasten\n")
      (insert (format "Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert "---\n\n")
      (if (string-empty-p results)
          (insert "No open tasks found.\n")
        (let ((lines (split-string results "\n" t)))
          (dolist (line lines)
            (when (string-match "\\(.+\\):\\([0-9]+\\):\\(.*\\)" line)
              (let ((file (match-string 1 line))
                    (lnum (match-string 2 line))
                    (task (string-trim (match-string 3 line))))
                (insert (format "- **%s:%s**\n  %s\n\n"
                                (file-name-nondirectory file)
                                lnum
                                task)))))))
      (goto-char (point-min))
      (markdown-mode)
      (read-only-mode 1))
    (switch-to-buffer buffer-name)))

;; Toggle markdown checkbox at point
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

(defvar zettelkasten-archive-directory (concat zettelkasten-directory "archive/")
  "Path to the archive directory for old notes.")

(defun zettelkasten-archive-note ()
  "Move the current note file to the archive subdirectory."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (file-name (file-name-nondirectory current-file)))
    (unless current-file
      (user-error "Buffer is not visiting a file"))
    (unless (string-suffix-p ".md" file-name)
      (user-error "Not a markdown file"))
    (unless (string-prefix-p (expand-file-name zettelkasten-directory)
                             (expand-file-name current-file))
      (user-error "File is not in the Zettelkasten directory"))
    ;; Don't archive files already in archive
    (when (string-prefix-p (expand-file-name zettelkasten-archive-directory)
                           (expand-file-name current-file))
      (user-error "File is already in archive"))
    ;; Ensure archive directory exists
    (unless (file-directory-p zettelkasten-archive-directory)
      (make-directory zettelkasten-archive-directory t))
    ;; Move the file
    (let ((new-path (concat zettelkasten-archive-directory file-name)))
      (when (file-exists-p new-path)
        (user-error "File already exists in archive: %s" file-name))
      (rename-file current-file new-path)
      (set-visited-file-name new-path)
      (save-buffer)
      (message "Archived: %s -> archive/" file-name))))

;; ============================================================================
;; Keybindings under SPC n r (org-roam leader)
;; ============================================================================

(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("r" . "roam")
        :desc "Find node" "f" #'org-roam-node-find
        :desc "Insert link" "i" #'zettelkasten-insert-wikilink
        :desc "Link region" "l" #'zettelkasten-link-region-or-insert
        :desc "Capture note" "c" #'org-roam-capture
        :desc "Toggle buffer" "b" #'org-roam-buffer-toggle
        :desc "Dailies today" "t" #'org-roam-dailies-goto-today
        :desc "Dailies capture" "d" #'org-roam-dailies-capture-today
        ;; Inbox workflow
        :desc "Open inbox" "I" #'zettelkasten-open-inbox
        :desc "Quick task" "q" #'zettelkasten-capture-to-inbox
        :desc "Quick note" "n" #'zettelkasten-capture-note-to-inbox
        ;; Task management
        :desc "Toggle checkbox" "x" #'zettelkasten-toggle-checkbox
        :desc "Archive done task" "a" #'zettelkasten-archive-done-task
        :desc "Archive note file" "A" #'zettelkasten-archive-note
        ;; Task review
        :desc "Review tasks (rg)" "R" #'zettelkasten-review-tasks
        :desc "Tasks buffer" "T" #'zettelkasten-tasks-buffer
        ;; Sync database
        :desc "Sync database" "s" #'org-roam-db-sync)))

;; ============================================================================
;; Markdown-mode enhancements for Zettelkasten
;; ============================================================================

(after! markdown-mode
  ;; Enable wikilink fontification
  (setq markdown-enable-wiki-links t
        markdown-wiki-link-search-type '(project))

  ;; Follow wikilinks in markdown files
  (defun zettelkasten-follow-wikilink-at-point ()
    "Follow the wikilink at point, searching for matching title in vault."
    (interactive)
    (let* ((link (markdown-link-url))
           (title (or link (thing-at-point 'symbol t))))
      (when title
        ;; Remove [[ and ]] if present
        (setq title (replace-regexp-in-string "^\\[\\[\\|\\]\\]$" "" title))
        ;; Search for file with matching title in YAML frontmatter
        (let* ((files (directory-files-recursively zettelkasten-directory "\\.md$"))
               (found nil))
          (dolist (file files)
            (unless found
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (when (re-search-forward (format "^title:.*\"%s\"" (regexp-quote title)) nil t)
                  (setq found file)))))
          (if found
              (find-file found)
            ;; If not found, offer to create
            (when (y-or-n-p (format "Note '%s' not found. Create it? " title))
              (org-roam-capture- :node (org-roam-node-create :title title))))))))

  ;; Keybinding to follow wikilinks
  (map! :map markdown-mode-map
        :n "gf" #'zettelkasten-follow-wikilink-at-point
        :n "RET" #'zettelkasten-follow-wikilink-at-point))
