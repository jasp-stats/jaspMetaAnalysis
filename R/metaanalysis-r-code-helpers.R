# Shared R-code rendering helpers.
#
# Converts dataset column names and string values to reproducible R code.

.metaAsRName <- function(name) {
  return(deparse(as.name(name), width.cutoff = 500))
}

.metaQuoteRString <- function(value) {
  # the backslashes are escaped first: the printed string evaluates to the value (a backslash is no escape sequence of the value)
  return(paste0("'", gsub("'", "\\\\'", gsub("\\", "\\\\", value, fixed = TRUE)), "'"))
}
