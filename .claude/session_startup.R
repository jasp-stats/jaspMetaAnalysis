# JASP Module - R Session Startup for Claude Code
# Run this script in your interactive R session (RStudio/Positron/radian)
# to prepare and hand over the session to Claude Code.
#
# Usage: source(".claude/session_startup.R")

# Fix cli::get_spinner() conflict with testthat in btw/evaluate context.
# BOTH the option AND the monkey-patch are needed:
#   - options(cli.spinner = "line") sets a sane default
#   - But btw:::local_reproducible_output() overrides it to FALSE on every
#     btw_tool_run_r() call, causing cli::get_spinner() to hit:
#       FALSE$frames -> "$ operator is invalid for atomic vectors"
#   - The monkey-patch intercepts logical values and coerces to "line"
# Upstream fixes pending: posit-dev/btw and r-lib/cli
options(cli.spinner = "line")
local({
  original_get_spinner <- cli::get_spinner
  patched_get_spinner <- function(which = NULL) {
    if (is.null(which)) {
      opt <- getOption("cli.spinner")
      if (identical(opt, FALSE) || identical(opt, TRUE)) {
        options(cli.spinner = "line")
      }
    } else if (identical(which, FALSE) || identical(which, TRUE)) {
      which <- "line"
    }
    original_get_spinner(which = which)
  }
  utils::assignInNamespace("get_spinner", patched_get_spinner, ns = "cli")
})

# Fix locale issue with renv.lock files created on non-English systems
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_ALL", "English_United States.utf8")
} else {
  Sys.setlocale("LC_ALL", "C.UTF-8")
}

# Install order matches container_entrypoint.sh:
# 1. Install injected packages (btw, mcptools) first
# 2. renv::restore() so lockfile-pinned versions win for shared deps
# 3. Install the module + jaspTools (already handled by restore for deps)
renv::install(c("btw", "mcptools"), prompt = FALSE)
renv::restore(prompt = FALSE)
renv::install(c(".", "jasp-stats/jaspTools"), prompt = FALSE)
library(jaspTools)
setupJaspTools()
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)
btw::btw_mcp_session()
