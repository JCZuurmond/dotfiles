# Agent Teams

Suggest using agent teams when a task has clearly independent workstreams that benefit from parallel exploration — e.g. research from multiple angles, parallel implementation of separate modules, or debugging with competing hypotheses. Don't suggest them for sequential tasks or simple single-file changes.

# Session Review

After finishing work, review the session traces to find opportunities to improve Claude configurations — e.g. hooks that could automate repetitive steps, skills that could capture useful workflows, rules for path-specific instructions, or prompt/instruction improvements based on friction points.

When suggesting improvements, propose the right scope: user-level (`~/.claude/`) for cross-project patterns, project-level (`.claude/`) for team-shared conventions, and local (`.claude/settings.local.json` or `CLAUDE.local.md`) for personal overrides.

# Commit Messages

- Keep the subject line under 50 characters and describe what changed
- Use imperative mood ("Add feature" not "Added feature")
- Use the commit body to explain why the change was made
