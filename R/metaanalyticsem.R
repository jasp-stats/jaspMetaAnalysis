#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

MetaAnalyticSem <- function(jaspResults, dataset, options, state = NULL) {

  # set OpenMx options
  # the JASP options() cleanup does not properly re-sets OpenMx settings
  # consequently, the fitting function crashes with matrix(byrow) error
  OpenMx::mxSetDefaultOptions()

  # read the data set
  dataset <- .masemReadData(dataset)

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  # estimate the models
  .masemFitModels(jaspResults, dataset, options)

  # create summary with model fit statistics (for all models)
  .masemModelFitTable(jaspResults, options)

  # create model-level summaries
  if (options[["modelSummary"]])
    .masemModelSummaryTable(jaspResults, options)

  # create path diagrams
  if (options[["pathDiagram"]])
    .masemModelPathDiagram(jaspResults, options)

  return()
}

.masemReadData                <- function(dataset) {

  if (!is.null(dataset))
    return(dataset)

  dataset <- .readDataSetToEnd(all.columns = TRUE)

  # check that all columns are numeric
  for(i in seq_len(ncol(dataset))) {
    dataset[,i] <- as.numeric(as.character(dataset[,i]))
  }

  return(dataset)
}

