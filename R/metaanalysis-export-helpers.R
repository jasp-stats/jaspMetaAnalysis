# Shared computed-column export helpers.
#
# Used by meta-analysis and effect-size workflows before creating JASP columns.

.metaValidateColumnName <- function(columnName) {

  if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
    .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

  return()
}
