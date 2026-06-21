## Commit Message Style

**Be extremely concise. Sacrifice grammar for concision.**

### Format:
```
<type>: <short description>

[optional body if needed]

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

### Types:
- `feat:` - New feature or analysis
- `fix:` - Bug fix
- `refactor:` - Code restructuring without behavior change
- `test:` - Adding or updating tests
- `docs:` - Documentation only
- `i18n:` - Translation updates
- `chore:` - Maintenance tasks

### Examples:
```
feat: add equivalence bounds plot

fix: correct CI calculation in paired t-test

test: update snapshots for descriptives table

refactor: extract common validation logic

i18n: update translation files
```

## Commit Workflow

### 0. Ensure on feature branch:
```bash
# Check current branch
git branch

# If on master, create feature branch
git checkout -b feature/descriptive-name
```

### 1. Before committing:
```bash
# Run full test suite
Rscript -e "library(jaspTools); agentTestAll()"

# Check git status
git status

# Review changes
git diff
```

### 2. Stage specific files:
```bash
# Stage specific files (preferred)
git add R/equivalenceonesamplettest.R
git add tests/testthat/test-equivalenceonesamplettest.R

# Avoid staging everything unless you're certain
# git add -A  # Be careful with this
```

### 3. Commit locally with co-author:
```bash
git commit -m "$(cat <<'EOF'
feat: add descriptives table

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
EOF
)"
```

**Local commits are OK. Pushing to remote requires human approval.**

## Pre-Commit Requirements

Before every commit, ensure:
- ✅ All tests pass (`jaspTools::agentTestAll()`)
- ✅ No unintended files staged (.env, credentials, etc.)
- ✅ Commit message is concise and descriptive
- ✅ Changes are focused and related

## Branch Strategy

- **Main branch:** `master`
- **NEVER work directly on `master` branch**
- **ALWAYS create a feature branch for any changes:**
  ```bash
  git checkout -b feature/descriptive-name
  ```
- Branch naming conventions:
  - `feature/description` - New features or analyses
  - `fix/description` - Bug fixes
  - `refactor/description` - Code restructuring
  - `test/description` - Test updates

## Pull Request Guidelines

**CRITICAL: NEVER push to remote, create PRs, or merge without explicit human approval.**

Human must review all local changes before they go online.

When human approves creating a PR:
1. Ensure all tests pass locally first
2. Keep PR scope focused and small
3. Use concise PR title (same style as commits)
4. Summarize changes in bullet points
5. Note any breaking changes
6. Wait for human to review the PR description before posting

## What NOT to Commit

- ❌ `.Rhistory`, `.RData`, `.Rproj.user/`
- ❌ Test artifacts or temporary files
- ❌ Personal IDE settings
- ❌ Large data files
- ❌ Credentials or API keys
- ❌ `CLAUDE.local.md` (personal preferences)

## CI/CD Integration

- GitHub Actions runs tests on every push
- Workflow file: `.github/workflows/unittests.yml`
- Tests must pass for PR to be merged
- Translation workflows run on schedule

## Git Safety

- **NEVER** work directly on `master` branch - always use feature branches
- **NEVER** push to remote without explicit human approval
- **NEVER** create pull requests without explicit human approval
- **NEVER** merge changes without explicit human approval
- **NEVER** force push to any branch
- **NEVER** amend published commits
- **NEVER** skip hooks unless explicitly needed
- **NEVER** commit without running tests first

**Human must approve all changes before they go online.**

## Common Git Commands

```bash
# Check current branch
git branch

# Create and switch to feature branch
git checkout -b feature/description

# Check status
git status

# View changes
git diff
git diff --staged

# Stage specific files
git add <file>

# Commit locally (OK to do without approval)
git commit -m "message"

# View recent commits
git log --oneline -5

# View commit history with graph
git log --graph --oneline --all -10

# === REQUIRE HUMAN APPROVAL BEFORE RUNNING: ===

# Push to remote (WAIT FOR APPROVAL)
git push origin feature/description

# Pull latest changes (usually safe, but confirm first)
git pull origin master
```

## Handling Test Failures

If CI tests fail after human has pushed:
1. Check GitHub Actions output
2. Reproduce failure locally
3. Fix the issue
4. Run tests to confirm fix
5. Commit locally
6. Ask human for approval to push fix

## Translation Commits

Translation updates are handled automatically:
- Weblate integration updates translation files
- Automated commits from translation workflow
- Don't manually edit translation files unless necessary
