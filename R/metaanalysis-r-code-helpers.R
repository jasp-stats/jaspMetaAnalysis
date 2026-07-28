# Shared R-code rendering helpers.
#
# Converts dataset column names and string values to reproducible R code.

.metaAsRName <- function(name) {
  return(deparse(as.name(name), width.cutoff = 500))
}

.metaQuoteRString <- function(value) {
  return(paste0("'", gsub("'", "\\\\'", value), "'"))
}
