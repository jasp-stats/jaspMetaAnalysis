# JASP Module - R Session Startup for Claude Code
# Run this script in your interactive R session (RStudio/Positron/radian)
# to prepare and hand over the session to Claude Code.
#
# Usage: source(".claude/session_startup.R")

# Fix cli::get_spinner() conflict with testthat in btw/evaluate context
options(cli.spinner = "line")

# Fix locale issue with renv.lock files created on non-English systems
Sys.setlocale("LC_ALL", "English_United States.utf8")

renv::restore(prompt = FALSE)
library(jaspTools)
renv::install(".", prompt = FALSE)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)
btw::btw_mcp_session()
