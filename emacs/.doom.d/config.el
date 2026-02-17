;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; ============================================================================
;;; CORE CONFIGURATION
;;; ============================================================================

;; Prefer newer changes
(setq load-prefer-newer t)

;; Ensure ~/.local/bin is on PATH (GUI Emacs doesn't inherit full shell PATH)
(let ((local-bin (concat (getenv "HOME") "/.local/bin")))
  (unless (string-match-p local-bin (or (getenv "PATH") ""))
    (setenv "PATH" (concat local-bin ":" (getenv "PATH"))))
  (add-to-list 'exec-path local-bin))

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

;; Vault configuration (defined early, used by neotree and zettelkasten sections)
(defvar zettelkasten-directory "~/org/"
  "Root directory for the Markdown Zettelkasten vault.")

;;; ============================================================================
;;; NEOTREE
;;; ============================================================================

;; Reverse-sort files in daily notes directory (newest first)
;; file-truename needed: neotree resolves symlinks, ~/org/ is a symlink
(after! neotree
  (defvar neo-reverse-sort-directories
    (list (file-truename (expand-file-name "daily/" zettelkasten-directory))
          (file-truename (expand-file-name "notes/" zettelkasten-directory)))
    "Directories where neotree should sort files in reverse alphabetical order.")

  (defun neo-filepath-sort-reverse-for-directories (a b)
    "Sort A and B alphabetically, but reverse in `neo-reverse-sort-directories'."
    (if (seq-some (lambda (dir)
                    (and (string-prefix-p dir a)
                         (string-prefix-p dir b)))
                  neo-reverse-sort-directories)
        (string> (downcase a) (downcase b))
      (string< (downcase a) (downcase b))))

  (setq neo-filepath-sort-function #'neo-filepath-sort-reverse-for-directories))

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

  ;; Define backends
  (defvar gptel-backend--copilot
    (gptel-make-gh-copilot "Copilot" :stream t)
    "GitHub Copilot backend for cloud models.")

  (defvar gptel-backend--ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '(qwen3-coder:30b
                qwen2.5-coder:7b
                llama3.2:latest))
    "Ollama backend for local models.")

  (defvar gptel-backend--anthropic nil
    "Anthropic backend for Claude API. Initialized lazily.")

  (defun gptel--init-anthropic-backend ()
    "Initialize Anthropic backend on first use."
    (unless gptel-backend--anthropic
      (setq gptel-backend--anthropic
            (gptel-make-anthropic "Anthropic"
              :key (gptel-api-key-from-auth-source "api.anthropic.com")
              :stream t
              :models '(claude-sonnet-4-20250514
                        claude-opus-4-20250514
                        claude-haiku-3-5-20241022))))
    gptel-backend--anthropic)

  ;; Core gptel configuration
  (setq gptel-model 'claude-sonnet-4
        gptel-default-mode 'org-mode
        gptel-use-curl t
        gptel-use-tools t
        gptel-confirm-tool-calls 'always
        gptel-include-tool-results 'auto
        gptel-stream t
        gptel-backend gptel-backend--copilot)

  ;; Quick backend switching
  (defun gptel-use-copilot ()
    "Switch to GitHub Copilot backend."
    (interactive)
    (setq gptel-backend gptel-backend--copilot
          gptel-model 'claude-sonnet-4)
    (message "Switched to Copilot (claude-sonnet-4)"))

  (defun gptel-use-ollama ()
    "Switch to local Ollama backend."
    (interactive)
    (setq gptel-backend gptel-backend--ollama
          gptel-model 'qwen3-coder:30b)
    (message "Switched to Ollama (qwen3-coder:30b)"))

  (defun gptel-use-anthropic ()
    "Switch to Anthropic Claude API backend."
    (interactive)
    (setq gptel-backend (gptel--init-anthropic-backend)
          gptel-model 'claude-sonnet-4-20250514)
    (message "Switched to Anthropic (claude-sonnet-4)")))

;; gptel-agent for agentic capabilities
(use-package! gptel-agent
  :after gptel
  :config
  (gptel-agent-update)

  ;; Project-aware system prompt
  (defun gptel-agent-project-prompt ()
    "Generate a system prompt with project context."
    (let* ((root (projectile-project-root))
           (project-name (when root (projectile-project-name)))
           (project-type (when root (projectile-project-type))))
      (concat
       "You are an expert software engineer assisting with "
       (if project-name (format "the %s project" project-name) "a software project") ". "
       (when project-type (format "This is a %s project. " project-type))
       "You have access to the codebase through the provided context. "
       "Be concise, write clean code, and follow the project's existing patterns. "
       "When modifying code, show only the relevant changes unless asked for full files.")))

  ;; Start agent chat with project context
  (defun gptel-agent-project ()
    "Start gptel-agent with project context preloaded."
    (interactive)
    (let ((root (projectile-project-root)))
      (unless root (user-error "Not in a project"))
      ;; Add project files to context
      (gptel-context-add-project)
      ;; Add current buffer if it's a code file
      (when (derived-mode-p 'prog-mode)
        (gptel-context-add-buffer))
      ;; Set project-aware prompt and start agent
      (setq-local gptel--system-message (gptel-agent-project-prompt))
      (call-interactively #'gptel-agent))))

;; Claude Code IDE integration
(use-package! claude-code-ide
  :commands (claude-code-ide claude-code-ide-menu)
  :config
  (setq claude-code-ide-cli-path "claude"
        claude-code-ide-terminal-backend 'vterm
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 90)

  ;; Unset CLAUDECODE env var so Claude doesn't refuse to start
  ;; when Emacs was launched from within a Claude Code terminal session
  (setenv "CLAUDECODE" nil)

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

;; Helper functions for AI tools - Context Management
(defun gptel-context-list ()
  "Display current gptel contexts in a buffer."
  (interactive)
  (if (not gptel-context)
      (message "No gptel contexts.")
    (with-current-buffer (get-buffer-create "*gptel-context*")
      (read-only-mode -1)
      (erase-buffer)
      (insert "=== gptel Context ===\n\n")
      (let ((total-chars 0))
        (dolist (context gptel-context)
          (let* ((source (car context))
                 (overlays (plist-get (cdr context) :overlays))
                 (source-name (cond ((bufferp source) (buffer-name source))
                                    ((stringp source) (file-name-nondirectory source))
                                    (t "Unknown"))))
            (insert (format "📄 %s\n" source-name))
            (cond
             ;; File-based context (string path)
             ((stringp source)
              (if (file-exists-p source)
                  (let ((chars (nth 7 (file-attributes source))))
                    (setq total-chars (+ total-chars (or chars 0)))
                    (insert (format "   (file: %d chars)\n" (or chars 0))))
                (insert "   (file not found)\n")))
             ;; Buffer with valid overlays
             ((and overlays (bufferp source) (buffer-live-p source))
              (dolist (overlay overlays)
                (when (overlay-buffer overlay)
                  (let ((chars (- (overlay-end overlay) (overlay-start overlay))))
                    (setq total-chars (+ total-chars chars))
                    (insert (format "   Lines %d-%d (%d chars)\n"
                                    (with-current-buffer source
                                      (line-number-at-pos (overlay-start overlay)))
                                    (with-current-buffer source
                                      (line-number-at-pos (overlay-end overlay)))
                                    chars))))))
             ;; Buffer closed or overlays invalid
             (t
              (insert "   (buffer closed - context lost)\n")))))
        (insert (format "\n─────────────────────\nTotal: %d chars (~%d tokens)\n"
                        total-chars (/ total-chars 4))))
      (read-only-mode 1)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun gptel-context-clear ()
  "Clear all gptel contexts."
  (interactive)
  (gptel-context-remove-all)
  (message "Cleared all gptel contexts."))

(defun gptel-context-add-buffer ()
  "Add current buffer to gptel context.
Uses file-based context for file-visiting buffers (persists after buffer close),
or overlay-based context for non-file buffers."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (progn
        (gptel-add-file file)
        (message "Added file %s to context." (file-name-nondirectory file)))
    (gptel-add (point-min) (point-max))
    (message "Added buffer %s to context (will be lost if buffer is closed)." (buffer-name))))

(defun gptel-context-add-project ()
  "Add key project files to gptel context.
Uses file-based context which persists even after buffers are closed."
  (interactive)
  (let* ((root (projectile-project-root))
         (files-added 0))
    (unless root
      (user-error "Not in a project"))
    ;; Add common project files if they exist
    (dolist (file '("README.md" "README.org" "README"
                    "pyproject.toml" "setup.py" "package.json"
                    "Cargo.toml" "go.mod" "Makefile"))
      (let ((path (expand-file-name file root)))
        (when (file-exists-p path)
          (gptel-add-file path)
          (setq files-added (1+ files-added)))))
    (message "Added %d project files to context." files-added)))

(defun gptel-context-add-related ()
  "Add files related to current buffer (imports, requires) to context.
Uses file-based context which persists even after buffers are closed."
  (interactive)
  (let ((files (gptel--get-related-files)))
    (if (not files)
        (message "No related files found.")
      (dolist (file files)
        (when (file-exists-p file)
          (gptel-add-file file)))
      (message "Added %d related files to context." (length files)))))

(defun gptel--get-related-files ()
  "Get list of files related to current buffer based on imports."
  (let ((files nil)
        (root (or (projectile-project-root) default-directory)))
    (save-excursion
      (goto-char (point-min))
      (cond
       ;; Python imports
       ((derived-mode-p 'python-mode)
        (while (re-search-forward "^\\(?:from\\|import\\) \\([a-zA-Z0-9_.]+\\)" nil t)
          (let* ((module (match-string 1))
                 (path (concat root (replace-regexp-in-string "\\." "/" module) ".py")))
            (when (file-exists-p path)
              (push path files)))))
       ;; JavaScript/TypeScript imports
       ((derived-mode-p 'js-mode 'typescript-mode 'js2-mode)
        (while (re-search-forward "\\(?:import\\|require\\)[^'\"]*['\"]\\([^'\"]+\\)['\"]" nil t)
          (let* ((module (match-string 1))
                 (path (expand-file-name module root)))
            (dolist (ext '("" ".js" ".ts" ".jsx" ".tsx"))
              (let ((full-path (concat path ext)))
                (when (file-exists-p full-path)
                  (push full-path files)))))))))
    (delete-dups files)))

;;; ============================================================================
;;; ZETTELKASTEN (MARKDOWN-BASED NOTE SYSTEM)
;;; ============================================================================

(defun zettelkasten-daily-file-template (&optional org-roam-p)
  "Return the template string for a daily file.
If ORG-ROAM-P is non-nil, return template with org-roam %<...> placeholders.
Otherwise, return template with current date evaluated."
  (let ((title (if org-roam-p "%<%Y-%m-%d>" (format-time-string "%Y-%m-%d")))
        (date (if org-roam-p "%<%Y-%m-%d>" (format-time-string "%Y-%m-%d")))
        (heading (if org-roam-p "%<%A, %B %d, %Y>" (format-time-string "%A, %B %d, %Y"))))
    (format "---\ntitle: \"%s\"\ndate: %s\ntags: [daily]\n---\n\n# %s\n\n## Tasks\n\n- [ ] \n\n## Notes\n\n"
            title date heading)))

;; org-roam configuration for Zettelkasten
(after! org-roam
  ;; Enable md-roam before configuring org-roam
  (require 'md-roam)
  (md-roam-mode 1)
  (setq md-roam-file-extension "md"
        md-roam-use-title-for-link-descr t)

  ;; Suppress org-element warnings in markdown buffers
  (advice-add 'org-element-at-point :around
              (lambda (orig-fn &rest args)
                (if (derived-mode-p 'markdown-mode)
                    nil
                  (apply orig-fn args))))

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
      (format-time-string (concat "%y%m%d-" slug))))

  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Generate a timestamped slug for NODE."
    (zettelkasten-slug (org-roam-node-title node)))

  ;; Capture templates for different note types
  ;; Trick below is to keep a small header and most of the template in one
  (setq org-roam-capture-templates
        '(("n" "note" plain
           "title: \"${title}\"\ndate: %<%Y-%m-%d>\ntags: [note]\n---\n\n# ${title}\n\n%?"
           :target (file+head "notes/${slug}.md" "---")
           :unnarrowed t)))

  ;; Daily notes
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "%<%y%m%d>.md"
                              ,(zettelkasten-daily-file-template t))
           :unnarrowed t))))

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
      (if (re-search-forward "^## Inbox" nil t)
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

(defun zettelkasten-archive-task ()
  "Archive the task at point to a dated file in the archive directory."
  (interactive)
  (require 'calendar)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (month (string-to-number (format-time-string "%m")))
         (year (string-to-number (format-time-string "%Y")))
         (last-day (calendar-last-day-of-month month year))
         (archive-file (concat zettelkasten-archive-directory
                               (format "%04d%02d%02d.md" year month last-day))))
    (unless (file-directory-p zettelkasten-archive-directory)
      (make-directory zettelkasten-archive-directory t))
    (delete-region (line-beginning-position)
                   (min (1+ (line-end-position)) (point-max)))
    (save-buffer)
    (with-current-buffer (find-file-noselect archive-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert line "\n")
      (save-buffer))
    (message "Archived to %s" (file-name-nondirectory archive-file))))

(defun zettelkasten-capture-git-commits ()
  "Capture today's git commits from current project to daily file.
Upserts: updates existing section for project or inserts new one."
  (interactive)
  (let* ((root (projectile-project-root))
         (project-name (when root (projectile-project-name)))
         (today (format-time-string "%Y-%m-%d"))
         (daily-file (concat zettelkasten-directory "daily/"
                             (format-time-string "%y%m%d") ".md"))
         (section-header (format "## Commits - %s" project-name))
         commits commit-text)
    (unless root
      (user-error "Not in a project"))
    ;; Get today's commits
    (setq commits
          (string-trim
           (shell-command-to-string
            (format "cd %s && git log --oneline --after='%s 00:00:00' --before='%s 23:59:59' 2>/dev/null"
                    (shell-quote-argument root) today today))))
    (if (string-empty-p commits)
        (message "No commits found for today in %s" project-name)
      ;; Build commit text
      (setq commit-text
            (concat section-header "\n\n"
                    (mapconcat (lambda (c) (format "- %s" c))
                               (split-string commits "\n" t)
                               "\n")
                    "\n"))
      ;; Ensure daily file exists (create with template if not)
      (unless (file-exists-p daily-file)
        (with-temp-file daily-file
          (insert (zettelkasten-daily-file-template))))
      ;; Upsert commits into daily file
      (with-current-buffer (find-file-noselect daily-file)
        (goto-char (point-min))
        (if (search-forward section-header nil t)
            ;; Update existing section
            (let ((section-start (line-beginning-position))
                  (section-end (save-excursion
                                 (forward-line 1)
                                 (if (re-search-forward "^## " nil t)
                                     (line-beginning-position)
                                   (point-max)))))
              (delete-region section-start section-end)
              (goto-char section-start)
              (insert commit-text "\n")
              (message "Updated %d commits for %s"
                       (length (split-string commits "\n" t))
                       project-name))
          ;; Insert new section at end
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n" commit-text)
          (message "Captured %d commits from %s"
                   (length (split-string commits "\n" t))
                   project-name))
        (save-buffer)))))

(defun zettelkasten-log-project-switch ()
  "Log current project to daily file on project switch.
Upserts: only adds project if not already listed.
Projects section is appended to the bottom of the file."
  (when-let* ((root (projectile-project-root))
              (project-name (projectile-project-name)))
    (let* ((daily-file (concat zettelkasten-directory "daily/"
                               (format-time-string "%y%m%d") ".md"))
           (section-header "## Projects")
           (project-entry (format "- [%s](%s)" project-name root)))
      ;; Ensure daily file exists
      (unless (file-exists-p daily-file)
        (with-temp-file daily-file
          (insert (zettelkasten-daily-file-template))))
      ;; Upsert project into daily file
      (with-current-buffer (find-file-noselect daily-file)
        (goto-char (point-min))
        ;; Check if project already logged
        (unless (search-forward project-entry nil t)
          (goto-char (point-min))
          (if (search-forward section-header nil t)
              ;; Add to existing Projects section (at end of section)
              (let ((section-end (save-excursion
                                   (forward-line 1)
                                   (if (re-search-forward "^## " nil t)
                                       (match-beginning 0)
                                     (point-max)))))
                (goto-char section-end)
                (skip-chars-backward "\n \t")
                (end-of-line)
                (insert "\n" project-entry))
            ;; Create new Projects section at the bottom
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n" section-header "\n\n" project-entry "\n"))
          (save-buffer))))))

(add-hook 'projectile-after-switch-project-hook #'zettelkasten-log-project-switch)

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
        :n "RET" #'zettelkasten-follow-wikilink-at-point
        (:localleader
         :desc "Toggle checkbox" "x" #'zettelkasten-toggle-checkbox
         :desc "Archive task" "a" #'zettelkasten-archive-task)))

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
       :desc "Open agentic chat" "a" #'gptel-agent
       :desc "Project agent" "p" #'gptel-agent-project
       :desc "Claude Code IDE" "C" #'claude-code-ide
       :desc "Menu" "m" #'gptel-menu
       :desc "Tools menu" "t" #'gptel-tools
       :desc "Rewrite region" "r" #'gptel-rewrite
       :desc "Gemini" "g" #'gemini-cli
       :desc "MCP hub" "h" #'mcp-hub
       :desc "Restart MCP" "R" #'mcp-hub-restart-all-server
       :desc "Use Copilot" "1" #'gptel-use-copilot
       :desc "Use Ollama" "2" #'gptel-use-ollama
       :desc "Use Anthropic" "3" #'gptel-use-anthropic
       (:prefix ("c" . "context")
        :desc "Add region/buffer" "a" #'gptel-add
        :desc "Add file" "f" #'gptel-add-file
        :desc "Add current buffer" "b" #'gptel-context-add-buffer
        :desc "Add project files" "p" #'gptel-context-add-project
        :desc "Add related files" "r" #'gptel-context-add-related
        :desc "List context" "l" #'gptel-context-list
        :desc "Clear all" "c" #'gptel-context-clear)))

;; Override default capture to use org-roam
(map! :leader
      :desc "Capture (org-roam)" "X" #'org-roam-capture)

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
        :desc "Sync database" "s" #'org-roam-db-sync
        :desc "Capture git commits" "g" #'zettelkasten-capture-git-commits)))
