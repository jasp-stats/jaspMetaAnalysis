# Claude Code Instructions

This directory contains project-specific instructions for Claude Code, Anthropic's CLI tool.

## Purpose

These files are automatically loaded when Claude Code starts, providing context about:

- JASP module structure and conventions
- Development workflows and best practices
- Testing requirements
- Translation guidelines

## Structure

```
.claude/
├── CLAUDE.md                          # Main project instructions (always loaded)
├── README.md                          # This file
├── mcp-server.R                       # MCP server startup script (R session tools)
├── settings.local.json                # Local Claude Code settings (not committed)
└── rules/                             # Path-specific rules
    ├── r-instructions.md              # R backend guidelines (**/R/*.R)
    ├── qml-instructions.md            # QML interface guidelines (**/inst/qml/*.qml)
    ├── testing-instructions.md        # Test framework guidelines (**/tests/testthat/*.R)
    ├── git-workflow.md                # Git and commit conventions
    └── translation-instructions.md    # i18n/l10n guidelines
```

## MCP Server Setup

The `.claude/mcp-server.R` script configures the `btw` MCP server for JASP module development. It:

1. Enables `btw_tool_run_r` for R code execution in a persistent session
2. Fixes `cli.spinner` option for testthat compatibility in the evaluate context
3. Exposes btw tool groups: docs, env, run, search, session

The user sets up their R session, then registers it via `btw::btw_mcp_session()`. Claude connects with `list_r_sessions` / `select_r_session` and executes R code in the user's session.

### Configuration

The MCP server is configured via `.mcp.json` in the module root (NOT committed to git). To set up:

```json
{
  "mcpServers": {
    "r-mcptools": {
      "type": "stdio",
      "command": "Rscript",
      "args": ["-e", "source('.claude/mcp-server.R')"]
    }
  }
}
```

Or via CLI: `claude mcp add r-mcptools -- Rscript -e "source('.claude/mcp-server.R')"`

### Connecting an Interactive R Session

To route MCP tool calls to your interactive R session (RStudio/Positron/radian):

```r
btw::btw_mcp_session()
```

This gives Claude Code access to your loaded objects and environment.

## How It Works

**Automatic Loading:**

- `CLAUDE.md` is automatically loaded in every Claude Code session
- Files in `rules/` are loaded based on their `paths:` frontmatter
- Path-specific rules apply only when working on matching files

**Path Scoping:**
Rules use YAML frontmatter to scope to specific files:

```yaml
---
paths:
  - "**/R/*.R"
---
```

## Copying to Other JASP Modules

To use these instructions in another JASP module:

1. Copy the `.claude/` directory to the target module
2. Create a `.mcp.json` in the module root (see Configuration above)
3. Adjust the `Rscript` command path if needed for your system
4. The `.mcp.json` file should be added to `.gitignore` (machine-specific paths)
5. The `.claude/mcp-server.R` script is portable and can be committed

## Personal Preferences

To add personal project-specific preferences that aren't shared with the team:

1. Create `CLAUDE.local.md` in this directory
2. Add your personal preferences
3. File is already in `.gitignore` and won't be committed

## Maintenance

**When to update:**

- Adding new development conventions
- Changing testing requirements
- Updating build/deployment processes
- Adding new repository-specific workflows

**What to include:**

- Information Claude can't infer from code
- Project-specific conventions that differ from defaults
- Critical commands and workflows
- Non-obvious patterns and gotchas

**What to exclude:**

- Standard language conventions
- Detailed API documentation (link to it instead)
- Frequently changing information
- Information easily discovered by reading code
