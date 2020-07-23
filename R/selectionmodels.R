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

SelectionModels <- function(jaspResults, dataset, options, state = NULL) {

  saveOptions(options)

  if(.SM_ready(options)){
    dataset <- .SM_data_get(dataset, options)
    .SM_data_check(dataset, options)
  }

  # fit the models
  if(.SM_ready(options)).SM_fit(jaspResults, dataset, options)
  
  # main summary tables
  .SM_fit_tests(jaspResults, dataset, options)
  .SM_fit_estimates(jaspResults, dataset, options)
  
  # the p-values frequency tables
  if(options[["p_table"]]).SM_p_frequency(jaspResults, dataset, options)
  
  # figures
  if(options[["FE_weightfunction"]]).SM_weights_plot(jaspResults, dataset, options, "FE")
  if(options[["RE_weightfunction"]]).SM_weights_plot(jaspResults, dataset, options, "RE")
  if(options[["plot_models"]]).SM_estimates_plot(jaspResults, dataset, options)
  
  return()
}


RDEBUG <- function(message){
  if(file.exists("D:/Projects/jasp/jasp-R-debug/RDEBUG.txt")){
    sink(file = "D:/Projects/jasp/jasp-R-debug/RDEBUG.txt", append = TRUE)
    cat(message)
    cat("\n")
    sink(file = NULL) 
  }
}
saveOptions <- function(options){
  if(file.exists("D:/Projects/jasp/jasp-R-debug/options.RDS"))
    saveRDS(options, file = "D:/Projects/jasp/jasp-R-debug/options.RDS")
}

.SM_dependencies <- c(
  "auto_reduce", "cutoffs_p", "selection_twosided",
  "input_ES", "input_SE", "input_p"
)
.SM_ready              <- function(options){
  return(options[["input_ES"]] != "" && options[["input_SE"]] != "")
}
.SM_data_get           <- function(dataset, options){
  if(!is.null(dataset)){
    return(dataset)
  }else{
    return(.readDataSetToEnd(columns.as.numeric = c(
      options[["input_ES"]],
      options[["input_SE"]],
      if(options[["input_p"]] != "") options[["input_p"]]
    )))
  }
}
.SM_data_check         <- function(dataset, options){
  
  .hasErrors(dataset               = dataset,
             type                  = c("infinity", "observations", "variance"),
             observations.amount   = "< 3",
             exitAnalysisIfErrors  = TRUE)
  
  .hasErrors(dataset               = dataset,
             type                  = c("negativeValues"),
             negativeValues.target = options[["input_SE"]],
             exitAnalysisIfErrors  = TRUE)
  
  # TODO: make this check work
  if(options[["input_p"]] != ""){
    .hasErrors(dataset              = dataset,
               type                 = c("limits"),
               min                  = 0,
               max                  = 1,
               limits.target        = options[["input_p"]],
               exitAnalysisIfErrors = TRUE)
  }
  
}
.SM_pcutoffs_get       <- function(options){
  
  x <- options[["cutoffs_p"]]
  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")
  
  x <- strsplit(x, ",", fixed = TRUE)[[1]]
  
  x <- trimws(x, which = "both")
  x <- x[x != ""]
  
  if(anyNA(as.numeric(x)))
    JASP:::.quitAnalysis(gettext("The p-value cuttoffs were set incorectly."))
  if(length(x) == 0){
    JASP:::.quitAnalysis(gettext("At least one p-value cuttoff needs to be set."))
  }
  
  x <- as.numeric(x)
  if(options[["selection_twosided"]]){
    x <- c(x/2, 1-x/2)
  }
  x <- c(sort(x), 1)
  
  return(x)
}
.SM_pval_get           <- function(dataset, options){
  # weightfunc uses one-sided p-values as input!
  if(options[["input_p"]] == ""){
    pval <- pnorm(dataset[,.v(options[["input_ES"]])]/dataset[,.v(options[["input_SE"]])], lower.tail = F) 
  }else{
    pval <- dataset[,.v(options[["input_p"]])]
  }
  return(pval)
}
.SM_autoreduce         <- function(steps, pval){

  # create p-value table
  cutoffs_table <- table(cut(pval, breaks = c(0, steps)))
  
  # remove the empty ones
  steps         <- steps[which(cutoffs_table != 0, arr.ind = TRUE)[,1]]
  
  # removed those with less than 3 p-values
  # remove them one-by one
  test <- TRUE
  while(test){
    cutoffs_table <- table(cut(pval, breaks = c(0, steps))) 
    
    lower_than_3  <- which(cutoffs_table <= 3, arr.ind = TRUE)[,1]
    if(length(lower_than_3) > 0){
      steps       <- steps[-which(cutoffs_table <= 3, arr.ind = TRUE)[1,1]] 
    }else{
      test        <- FALSE
    }
  }

  # do not fit the models if there is only one p-value interval
  if(length(steps) <= 1){
    stop("No steps")
  }
  
  return(steps)
}
.SM_fill_estimates     <- function(table, fit, type = "FE"){
  
  if(type == "FE"){
    mean_pos <- 1    
  }else{
    mean_pos <- 2
  }
  CI_overtitle <- gettext("95% Confidence Interval")
  
  table$addColumnInfo(name = "type", title = "",                        type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",   title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = CI_overtitle)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = CI_overtitle)
  
  row_unadjusted <- list(type = "Unadjusted")
  row_adjusted   <- list(type = "Adjusted")
  
  if(!is.null(fit)){
    if(!class(fit) %in% c("simpleError","error")){
      row_unadjusted    <- c(row_unadjusted, list(
        est  = fit[["unadj_est"]][mean_pos,1],
        se   = fit[["unadj_se"]][mean_pos,1],
        stat = fit[["z_unadj"]][mean_pos,1],
        pval = fit[["p_unadj"]][mean_pos,1],
        lCI  = fit[["ci.lb_unadj"]][mean_pos,1],
        uCI  = fit[["ci.ub_unadj"]][mean_pos,1]
      ))
      row_adjusted    <- c(row_adjusted, list(
        est  = fit[["adj_est"]][mean_pos,1],
        se   = fit[["adj_se"]][mean_pos,1],
        stat = fit[["z_adj"]][mean_pos,1],
        pval = fit[["p_adj"]][mean_pos,1],
        lCI  = fit[["ci.lb_adj"]][mean_pos,1],
        uCI  = fit[["ci.ub_adj"]][mean_pos,1]
      ))
      
      warning_messages <- .SM_warning_messages(fit)
      for(i in seq_along(warning_messages)){
        table$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
      }
      
    }else{
      table$setError(.SM_error_message(fit))
    }    
  }
  
  table$addRows(row_unadjusted)
  table$addRows(row_adjusted)

  return(table)
}
.SM_fill_heterogeneity <- function(table, fit){

  CI_overtitle <- gettext("95% Confidence Interval")
  
  table$addColumnInfo(name = "type", title = "",                        type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = CI_overtitle)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = CI_overtitle)
  
  row_RE_unadjusted_tau <- list(type = "Unadjusted")
  row_RE_adjusted_tau   <- list(type = "Adjusted")

  if(!is.null(fit)){
    if(!class(fit) %in% c("simpleError","error")){
      row_RE_unadjusted_tau  <- c(row_RE_unadjusted_tau, list(
        est  = sqrt(fit[["unadj_est"]][1,1]),
        stat = fit[["z_unadj"]][1,1],
        pval = fit[["p_unadj"]][1,1],
        lCI  = sqrt(fit[["ci.lb_unadj"]][1,1]),
        uCI  = sqrt(fit[["ci.ub_unadj"]][1,1])
      ))
      row_RE_adjusted_tau    <- c(row_RE_adjusted_tau, list(
        est  = sqrt(fit[["adj_est"]][1,1]),
        stat = fit[["z_adj"]][1,1],
        pval = fit[["p_adj"]][1,1],
        lCI  = sqrt(fit[["ci.lb_adj"]][1,1]),
        uCI  = sqrt(fit[["ci.ub_adj"]][1,1])
      ))
      
      warning_messages <- .SM_warning_messages(fit)
      for(i in seq_along(warning_messages)){
        table$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
      }
      
    }else{
      table$setError(.SM_error_message(fit)) 
    }
  }
  
  table$addRows(row_RE_unadjusted_tau)
  table$addRows(row_RE_adjusted_tau)
  
  return(table)
}
.SM_fill_weights       <- function(table, fit, type = "FE"){
  
  CI_overtitle <- gettext("95% Confidence Interval")
  p_overtitle  <- gettext("<em>p</em>-values interval (one-sided)")
  
  if(type == "FE"){
    weights_add <- 0  
  }else{
    weights_add <- 1
  }
  
  table$addColumnInfo(name = "lr",   title = gettext("Lower"),          type = "number", overtitle = p_overtitle)
  table$addColumnInfo(name = "ur",   title = gettext("Upper"),          type = "number", overtitle = p_overtitle)
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",   title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = CI_overtitle)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = CI_overtitle)
  
  if(!is.null(fit)){
    if(!class(fit) %in% c("simpleError","error")){
      
      for(i in 1:length(fit[["steps"]])){
        if(i == 1){
          temp_row <- list(
            lr   = 0,
            ur   = fit[["steps"]][1],
            est  = 1,
            se   = 0,
            lCI  = 1,
            uCI  = 1
          )           
        }else{
          temp_row <- list(
            lr   = fit[["steps"]][i-1],
            ur   = fit[["steps"]][i],
            est  = fit[["adj_est"]][i+weights_add,1],
            se   = fit[["adj_se"]][i+weights_add,1],
            stat = fit[["z_adj"]][i+weights_add,1],
            pval = fit[["p_adj"]][i+weights_add,1],
            lCI  = ifelse(fit[["ci.lb_adj"]][i+weights_add,1] < 0, 0, fit[["ci.lb_adj"]][i+weights_add,1]),
            uCI  = fit[["ci.ub_adj"]][i+weights_add,1]
          ) 
        }
        table$addRows(temp_row)
      }
      
      warning_messages <- .SM_warning_messages(fit)
      for(i in seq_along(warning_messages)){
        table$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
      }
      
    }else{
      table$setError(.SM_error_message(fit)) 
    }
  }
  
  return(table)
}
.SM_fit                <- function(jaspResults, dataset, options){
  
  if(!is.null(jaspResults[["models"]])){
    return()
  }else{
    models <- createJaspState()
    models$dependOn(c(.SM_dependencies, "p_table"))
    jaspResults[["models"]] <- models
  }
  
  # get the p-value steps
  steps <- .SM_pcutoffs_get(options)
   
  # get the p-values
  pval  <- .SM_pval_get(dataset, options)
  
  # remove intervals that do not contain enought (3) p-values
  if(options[["auto_reduce"]]){
    steps <- tryCatch(.SM_autoreduce(steps, pval), error = function(e)e)

    if(class(steps) %in% c("simpleError","error")){
      models[["object"]] <- list(
        FE = steps,
        RE = steps
      )
      return()
    }
  }

  fit_FE <- tryCatch(weightr::weightfunct(
    effect = dataset[,.v(options[["input_ES"]])],
    v      = dataset[,.v(options[["input_SE"]])]^2,
    pval   = pval,
    steps  = steps,
    fe     = TRUE
  ),error = function(e)e)
  
  fit_RE <- tryCatch(weightr::weightfunct(
    effect = dataset[,.v(options[["input_ES"]])],
    v      = dataset[,.v(options[["input_SE"]])]^2,
    pval   = pval,
    steps  = steps,
    fe     = FALSE
  ),error = function(e)e)
  
  models[["object"]] <- list(
   FE = fit_FE,
   RE = fit_RE
  )

  return()
}
.SM_fit_tests          <- function(jaspResults, dataset, options){
  
  if(!is.null(jaspResults[["fit_tests"]])){
    return()
  }else{
    # create container
    fit_tests <- createJaspContainer(title = gettext("Model Tests"))
    fit_tests$position <- 1
    fit_tests$dependOn(.SM_dependencies)
    jaspResults[["fit_tests"]] <- fit_tests
  }
  
  models   <- jaspResults[["models"]]$object
  FE_error <- class(models$FE) %in% c("simpleError","error")
  RE_error <- class(models$RE) %in% c("simpleError","error")
  
  
  ### test of heterogeneity
  heterogeneity_test <- createJaspTable(title = gettext("Test of Heterogeneity"))
  heterogeneity_test$position <- 1
  fit_tests[["heterogeneity_test"]] <- heterogeneity_test
  
  heterogeneity_test$addColumnInfo(name = "stat",  title = gettext("Q"),  type = "number")
  heterogeneity_test$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
  heterogeneity_test$addColumnInfo(name = "pval",  title = gettext("p"),  type = "pvalue")
  
  if(!is.null(models)){
    
    row_heterogeneity      <- list()

    if(!FE_error){
      row_heterogeneity    <- c(row_heterogeneity, list(
        stat = models[["FE"]][["QE"]],
        df   = (models[["FE"]][["k"]] - models[["FE"]][["npred"]] - 1),
        pval = models[["FE"]][["QEp"]]
      ))
    }else if(!RE_error){
      row_heterogeneity    <- c(row_heterogeneity, list(
        stat = models[["RE"]][["QE"]],
        df   = (models[["RE"]][["k"]] - models[["RE"]][["npred"]] - 1),
        pval = models[["RE"]][["QEp"]]
      ))
    }
    
    heterogeneity_test$addRows(row_heterogeneity)
    
    warning_messages <- unique(c(
      .SM_warning_messages(models[["FE"]]), .SM_warning_messages(models[["RE"]])
    ))
    error_messages   <- unique(c(
      .SM_error_message(models[["FE"]], "FE"), .SM_error_message(models[["RE"]], "RE")
    ))
    for(i in seq_along(warning_messages)){
      heterogeneity_test$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
    }
    for(i in seq_along(error_messages)){
      heterogeneity_test$addFootnote(symbol = gettext("Error:"),   error_messages[i])
    }
  }
  
  
  ### test of bias
  bias_test <- createJaspTable(title = gettext("Test of Publication Bias"))
  bias_test$position <- 2
  fit_tests[["bias_test"]] <- bias_test
  
  bias_test$addColumnInfo(name = "type",  title = "",                type = "string")
  bias_test$addColumnInfo(name = "stat",  title = gettext("ChiSq"),  type = "number")
  bias_test$addColumnInfo(name = "df",    title = gettext("df"),     type = "integer")
  bias_test$addColumnInfo(name = "pval",  title = gettext("p"),      type = "pvalue")
  
  if(!is.null(models)){
    
    row_bias_homogeneity   <- list(type = gettext("Assuming homogeneity"))
    row_bias_heterogeneity <- list(type = gettext("Assuming heterogeneity"))
    
    if(!FE_error){
      row_bias_homogeneity <- c(row_bias_homogeneity, list(
        stat = 2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]),
        df   = length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
        pval = pchisq(
          2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]), 
          length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
          )
      ))
    }
    if(!RE_error){
      row_bias_heterogeneity <- c(row_bias_heterogeneity, list(
        stat = 2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]),
        df   = length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
        pval = pchisq(
          2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]), 
          length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }
    
    bias_test$addRows(row_bias_homogeneity)
    bias_test$addRows(row_bias_heterogeneity)
    
    warning_messages <- unique(c(
      .SM_warning_messages(models[["FE"]]), .SM_warning_messages(models[["RE"]])
    ))
    error_messages   <- unique(c(
      .SM_error_message(models[["FE"]], "FE"), .SM_error_message(models[["RE"]], "RE")
    ))
    for(i in seq_along(warning_messages)){
      bias_test$addFootnote(symbol = gettext("Warning:"), warning_messages[i])      
    }
    for(i in seq_along(error_messages)){
      bias_test$addFootnote(symbol = gettext("Error:"),   error_messages[i])
    }

  }
  
  return()
}
.SM_fit_estimates      <- function(jaspResults, dataset, options){
  
  models   <- jaspResults[["models"]]$object

  ### assuming homogeneity
  if(is.null(jaspResults[["FE_estimates"]])){
    # create container
    FE_estimates <- createJaspContainer(title = gettext("Fixed Effects Estimates"))
    FE_estimates$position <- 2
    FE_estimates$dependOn(c(.SM_dependencies, "FE_estimates"))
    jaspResults[["FE_estimates"]] <- FE_estimates
  }else{
    FE_estimates <- jaspResults[["FE_estimates"]]    
  }
  
  # mean estimates
  if(is.null(FE_estimates[["FE_estimates"]]) && options[["FE_estimates"]]){
    FE_estimates_mean <- createJaspTable(title = gettext("Mean Estimates"))
    FE_estimates_mean$position  <- 1
    FE_estimates[["FE_mean"]] <- FE_estimates_mean
    FE_mean <- .SM_fill_estimates(FE_estimates_mean, models[["FE"]], "FE")    
  }

  # weights estimates
  if(is.null(FE_estimates[["FE_weights"]]) && options[["FE_weights"]] && options[["FE_estimates"]]){
    FE_weights <- createJaspTable(title = gettext("Estimated Weights"))
    FE_weights$position  <- 2
    FE_weights$dependOn(c("FE_weights"))
    FE_estimates[["FE_weights"]] <- FE_weights
    FE_weights <- .SM_fill_weights(FE_weights, models[["FE"]], "FE")
  }
  
  
  ### assuming heterogeneity
  if(is.null(jaspResults[["RE_estimates"]])){
    # create container
    RE_estimates <- createJaspContainer(title = gettext("Random Effects Estimates"))
    RE_estimates$position <- 3
    RE_estimates$dependOn(c(.SM_dependencies, "RE_estimates"))
    jaspResults[["RE_estimates"]] <- RE_estimates
  }else{
    RE_estimates <- jaspResults[["RE_estimates"]]    
  }
  
  # mean estimates
  if(is.null(RE_estimates[["RE_mean"]]) && options[["RE_estimates"]]){
    RE_estimates_mean <- createJaspTable(title = gettext("Mean Estimates"))
    RE_estimates_mean$position <- 1
    RE_estimates[["RE_mean"]] <- RE_estimates_mean
    RE_estimates_mean <- .SM_fill_estimates(RE_estimates_mean, models[["RE"]], "RE")    
  }

  # tau estimates
  if(is.null(RE_estimates[["RE_estimates_tau"]]) && options[["RE_heterogeneity"]] && options[["RE_estimates"]]){
    RE_estimates_tau <- createJaspTable(title = gettextf("Heterogeneity Estimates (%s)", "\u03C4"))
    RE_estimates_tau$position <- 2
    RE_estimates_tau$dependOn(c("RE_heterogeneity"))
    RE_estimates[["RE_estimates_tau"]] <- RE_estimates_tau
    RE_estimates_tau <- .SM_fill_heterogeneity(RE_estimates_tau, models[["RE"]])    
  }

  # weights estimates
  if(is.null(RE_estimates[["RE_weights"]]) && options[["RE_weights"]] && options[["RE_estimates"]]){
    RE_weights <- createJaspTable(title = gettext("Estimated Weights"))
    RE_weights$position  <- 3
    RE_weights$dependOn(c("RE_weights"))
    RE_estimates[["RE_weights"]] <- RE_weights
    RE_weights <- .SM_fill_weights(RE_weights, models[["RE"]], "RE")
  }  
  
  return()
}
.SM_p_frequency        <- function(jaspResults, dataset, options){
  
  if(!is.null(jaspResults[["p_frequency"]])){
    return()
  }else{
    # create container
    p_frequency <- createJaspTable(title = gettext("P-values Frequency"))
    p_frequency$position <- 4
    p_frequency$dependOn(c(.SM_dependencies, "p_table"))
    jaspResults[["p_frequency"]] <- p_frequency
  }
  
  overtitle <- gettext("<em>p</em>-values interval (one-sided)")
  p_frequency$addColumnInfo(name = "lowerRange", title = gettext("Lower"),     type = "number", overtitle = overtitle)
  p_frequency$addColumnInfo(name = "upperRange", title = gettext("Upper"),     type = "number", overtitle = overtitle)
  p_frequency$addColumnInfo(name = "frequency",  title = gettext("Frequency"), type = "integer")
  
  if(!.SM_ready(options)){
    return()
  }
  
  models <- jaspResults[["models"]]$object
  
  # get the p-value steps and p-values (so we don't have to search for them in the models)
  steps <- .SM_pcutoffs_get(options)
  pval  <- .SM_pval_get(dataset, options)
  
  # add a note in case that the models failed to conver due to autoreduce
  if(class(models[["FE"]]) %in% c("simpleError","error") || class(models[["RE"]]) %in% c("simpleError","error")){
    
    if(options[["auto_reduce"]]){
      if(class(models[["FE"]]) %in% c("simpleError","error") && class(models[["RE"]]) %in% c("simpleError","error")){
        if(models[["FE"]]$message == "No steps"){
          p_frequency$addFootnote(gettext("There were no p-values cutoffs after their automatic reduction. The displayed frequencies correspond to the non-reduced p-values cuttoffs."))
        }
      }else{
        # the failure wasn't due to the reduce - reduce the p-values
        steps <- .SM_autoreduce(steps, pval)
      }
    }
  }else{
    if(options[["auto_reduce"]]){
      steps <- .SM_autoreduce(steps, pval)      
    }
  }
  
  steps <- c(0, steps)
  cutoffs_table <- table(cut(pval, breaks = steps))
  
  for(i in 1:length(cutoffs_table)){
    p_frequency$addRows(list(
      lowerRange = steps[i],
      upperRange = steps[i+1],
      frequency  = cutoffs_table[i]
    ))
  }
  
  return()
}
.SM_weights_plot       <- function(jaspResults, dataset, options, type = "FE"){
  
  if(!is.null(jaspResults[[paste0(type, "_weights")]])){
    return()
  }else{
    weights_plot <- createJaspPlot(
      title  = gettextf(
        "Weight Function (%s)",
        ifelse(type == "FE", gettext("Fixed Effects"), gettext("Random Effects"))
      ),
      width  = 500,
      height = 400)
    weights_plot$dependOn(c(.SM_dependencies, ifelse(type == "FE", "FE_weightfunction", "RE_weightfunction")))
    weights_plot$position <- ifelse(type == "FE", 5, 6)
    jaspResults[[paste0(type, "_weights")]] <- weights_plot
  }
  
  if(!.SM_ready(options)){
    return()
  }
  
  # handle errors
  fit <- jaspResults[["models"]]$object[[type]]
  if(class(fit) %in% c("simpleError","error")){
    weights_plot$setError(.SM_error_message(fit))
    return()
  }
  
  # get the weights and steps
  steps        <- c(0, fit[["steps"]])
  weights_mean <- c(1, fit[["adj_est"]][  ifelse(type == "FE", 2, 3):nrow(fit[["adj_est"]]),  1])
  weights_lCI  <- c(1, fit[["ci.lb_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.lb_adj"]]),1])
  weights_uCI  <- c(1, fit[["ci.ub_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.ub_adj"]]),1])
  
  # handle NaN in the estimates
  if(any(c(is.nan(weights_mean), is.nan(weights_lCI), is.nan(weights_uCI)))){
    weights_plot$setError(gettext("The figure could not be created since one of the estimates is NaN."))
  }
  
  # correct the lower bound
  weights_lCI[weights_lCI < 0] <- 0
  
  # get the ordering for plotting
  coord_order <- sort(rep(1:(length(steps)-1),2), decreasing = FALSE)
  steps_order <- c(1, sort(rep(2:(length(steps)-1), 2)), length(steps))
  
  # axis ticks
  x_tics    <- trimws(steps, which = "both", whitespace = "0")
  x_tics[1] <- 0
  y_tics    <- JASPgraphs::getPrettyAxisBreaks(range(c(weights_mean, weights_lCI, weights_uCI)))
  
  # make the plot happen
  plot <- ggplot2::ggplot()
  # mean
  plot <- plot + ggplot2::geom_polygon(
    ggplot2::aes(
      x = c(steps[steps_order], rev(steps[steps_order])),
      y = c(weights_lCI[coord_order], rev(weights_uCI[coord_order]))
    ),
    fill = "grey80")
  # CI
  plot <- plot +ggplot2::geom_path(
    ggplot2::aes(
      x = steps[steps_order],
      y = weights_mean[coord_order]
    ),
    size = 1.25)

  plot <- plot + ggplot2::scale_x_continuous(
    gettext("p-value (one-sided)"),
    breaks = steps,
    labels = x_tics,
    limits = c(0,1))
  plot <- plot + ggplot2::scale_y_continuous(
    gettext("Publication probability"),
    breaks = y_tics,
    limits = range(y_tics))
  
  plot <- JASPgraphs::themeJasp(plot)
  weights_plot$plotObject <- plot
  
  return()
}
.SM_estimates_plot     <- function(jaspResults, dataset, options){

  if(!is.null(jaspResults[["plot_estimates"]])){
    return()
  }else{
    plot_estimates <- createJaspPlot(
      title  = gettextf("Models' mean estimates"),
      width  = 500,
      height = 400)
    plot_estimates$dependOn(c(.SM_dependencies, "plot_models"))
    plot_estimates$position <- 7
    jaspResults[["plot_estimates"]] <- plot_estimates
  }
  
  if(!.SM_ready(options)){
    return()
  }
  
  # handle errors
  FE <- jaspResults[["models"]]$object[["FE"]]
  RE <- jaspResults[["models"]]$object[["RE"]]
  
  if(class(FE) %in% c("simpleError","error")){
    plot_estimates$setError(.SM_error_message(FE))
    return()
  }
  if(class(RE) %in% c("simpleError","error")){
    plot_estimates$setError(.SM_error_message(RE))
    return()
  }
  
  # get the estimates
  estimates <- data.frame(
    model = c(gettext("Fixed effects"),   gettext("Fixed effects (adjusted)"),   gettext("Random effects"),  gettext("Random effects (adjusted)")),
    mean  = c(FE[["unadj_est"]][1,1],     FE[["adj_est"]][1,1],                  RE[["unadj_est"]][2,1],     RE[["adj_est"]][2,1]),
    lCI   = c(FE[["ci.lb_unadj"]][1,1],   FE[["ci.lb_adj"]][1,1],                RE[["ci.lb_unadj"]][2,1],   RE[["ci.lb_adj"]][2,1]),
    uCI   = c(FE[["ci.ub_unadj"]][1,1],   FE[["ci.ub_adj"]][1,1],                RE[["ci.ub_unadj"]][2,1],   RE[["ci.ub_adj"]][2,1])
  )
  estimates <- estimates[4:1,]

  # handle NaN in the estimates
  if(any(c(is.nan(estimates[,"mean"]), is.nan(estimates[,"lCI"]), is.nan(estimates[,"uCI"])))){
    plot_estimates$setError(gettext("The figure could not be created since one of the estimates is NaN."))
  }
  
  # make the plot happen
  plot <- ggplot2::ggplot()
 
  plot <- plot + ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = estimates[,"lCI"],
      xmax = estimates[,"uCI"],
      y    = 1:4
    ))
  plot   <- plot + ggplot2::geom_point(
    ggplot2::aes(
      x = estimates[,"mean"],
      y = 1:4),
    shape = 15)
  plot <- plot + ggplot2::geom_line(ggplot2::aes(x = c(0,0), y = c(.5, 4.5)), linetype = "dotted")
  plot <- plot + ggplot2::scale_x_continuous(
    gettext("Mean estimate"),
    breaks = JASPgraphs::getPrettyAxisBreaks(range(c(0, estimates[,"lCI"], estimates[,"uCI"]))),
    limits = range(c(0, estimates[,"lCI"], estimates[,"uCI"])))
  plot <- plot + ggplot2::scale_y_continuous(
    "",
    breaks = 1:4,
    labels = estimates[,"model"],
    limits = c(0.5, 4.5))
  plot <- plot + ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank()
  )
  
  plot <- JASPgraphs::themeJasp(plot, sides = "b")
  plot_estimates$plotObject <- plot
  
  return()
}
.SM_error_message      <- function(fit, type = NULL){
  
  if(!is.null(type)){
    model_type <- switch(
      type,
      "FE" = gettext("Fixed effects model: "),
      "RE" = gettext("Random effects model: ")
    )    
  }else{
    model_type <- ""
  }

  # add more error messages as we find them I guess
  if(fit$message == "non-finite value supplied by optim"){
    message <- gettextf("%sThe optimizer failed to find a solution. Consider re-specifying the model.", model_type)
  }else{
    message <- paste0(model_type, fit$message)
  }

  return(message)
}
.SM_warning_messages   <- function(fit){
  
  messages   <- NULL

  if(!class(fit) %in% c("simpleError","error")){
    
    # check for no p-values in cuttoffs
    steps <- c(0, fit[["steps"]])
    pval  <- fit[["p"]]
    
    cutoffs_table <- table(cut(pval, breaks = steps))
    if(any(cutoffs_table == 0)){
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains no effect sizes, leading to estimation problems. Consider re-specifying the cutoffs."
      ))
    }else if(any(cutoffs_table <= 3)){
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains three or fewer effect sizes, which may lead to estimation problems. Consider re-specifying the cutoffs."
      ))
    }
    
  }

  return(messages)
}
