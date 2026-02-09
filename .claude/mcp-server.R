# Custom MCP server for JASP modules
# Provides btw tools with JASP-specific fixes
options(
  btw.run_r.enabled = TRUE,
  # Fix cli::get_spinner() returning FALSE in evaluate context,
  # which breaks testthat reporter initialization (which$frames error)
  cli.spinner = "line"
)

btw::btw_mcp_server(
  tools = btw::btw_tools("docs", "env", "run", "search", "session")
)
