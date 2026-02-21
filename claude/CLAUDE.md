# CI Practice (Trunk-Based Development)

Work ONLY on the main branch. Never create feature branches. This is non-negotiable.

Workflow for every change:
1. Pull before starting: `git pull --rebase origin main`
2. Make a small, focused change
3. Run the project's tests and linters (see project CLAUDE.md)
4. Pull again before committing: `git pull --rebase origin main`
5. Commit with a clear message
6. Push immediately: `git push origin main`
7. Repeat — never accumulate uncommitted work

If the build breaks after pushing:
- Fixing it is the HIGHEST priority, above all other work
- Either fix forward or revert the breaking commit
- Do not start new work until main is green

Never use `git checkout -b`, `git switch -c`, `git branch <name>`, or worktrees.

# Agent Teams

Suggest using agent teams when a task has clearly independent workstreams that benefit from parallel exploration — e.g. research from multiple angles, parallel implementation of separate modules, or debugging with competing hypotheses. Don't suggest them for sequential tasks or simple single-file changes.

# Commit Messages

- Keep the subject line under 50 characters and describe what changed
- Use imperative mood ("Add feature" not "Added feature")
- Use the commit body to explain why the change was made
