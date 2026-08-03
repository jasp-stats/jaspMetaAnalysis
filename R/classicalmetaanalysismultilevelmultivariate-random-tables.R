# Multilevel/multivariate random-effect tables.
#
# Builds random-estimate, inclusion-test, and confidence-interval tables.

.mammRandomEstimatesTable        <- function(jaspResults, options) {

  # obtain the overall container
  if (!is.null(jaspResults[["randomEstimatesContainer"]])) {
    randomEstimatesContainer <- jaspResults[["randomEstimatesContainer"]]
  } else {
    randomEstimatesContainer <- createJaspContainer(title = gettext("Random Effects / Model Stucture Summary"))
    randomEstimatesContainer$dependOn(.maDependencies)
    randomEstimatesContainer$position <- 2
    jaspResults[["randomEstimatesContainer"]] <- randomEstimatesContainer
  }

  fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

  if (options[["subgroup"]] == "") {

    # directly fill the main container if only full estimate is requested
    .mammRandomEstimatesTableFun(jaspResults, randomEstimatesContainer, options, fit[[1]])

  } else {

    for (i in seq_along(fit)) {

      # create subgroup containers
      if (!is.null(randomEstimatesContainer[[attr(fit[[i]], "subgroup")]])) {
        randomEstimatesSubgroupContainer <- randomEstimatesContainer[[attr(fit[[i]], "subgroup")]]
      } else {
        randomEstimatesSubgroupContainer <- createJaspContainer(title = gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup")))
        randomEstimatesSubgroupContainer$position <- 1
        randomEstimatesContainer[[attr(fit[[i]], "subgroup")]] <- randomEstimatesSubgroupContainer
      }

      # fill the subgroup containers
      .mammRandomEstimatesTableFun(jaspResults, randomEstimatesSubgroupContainer, options, fit[[i]])
    }
  }

  return()
}

.mammRandomEstimatesTableFun     <- function(jaspResults, randomEffectsContainer, options, fit) {

  dataset <- attr(fit, "dataset")

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  ### create table for nested random effects
  if (fit[["withS"]] && is.null(randomEffectsContainer[["containerS"]])) {

    containerS <- createJaspContainer(title = gettext("Simple / Nested Summary"))
    containerS$position <- 1
    randomEffectsContainer[["containerS"]] <- containerS

    tableS <- createJaspTable(title = gettext("Estimates"))
    tableS$position <- 1

    tableS$addColumnInfo(name = "factor",  type = "string",  title = "")
    tableS$addColumnInfo(name = "sigma2",  type = "number",  title = gettext("\U03C3\U00B2"))
    tableS$addColumnInfo(name = "sigma",   type = "number",  title = gettext("\U03C3"))
    tableS$addColumnInfo(name = "nlvls",   type = "integer", title = gettext("Levels"))
    if (.mammAddIsFixedRandom(options, 3))
      tableS$addColumnInfo(name = "fixed",   type = "string",  title = gettext("Fixed"))

    # tableS$addColumnInfo(name = "R",       type = "string",  title = gettext("R")) # whether supplied via known correlation matrix
    containerS[["tableS"]] <- tableS

    resultsS <- data.frame(
      factor = .maVariableNames(fit[["s.names"]], unlist(.mammExtractRandomVariableNames(options))),
      sigma  = sqrt(fit[["sigma2"]]),
      sigma2 = fit[["sigma2"]],
      nlvls  = fit[["s.nlevels"]],
      fixed  = ifelse(fit[["vc.fix"]]$sigma2, "yes", "no")
      # R      = ifelse(fit[["Rfix"]] , "yes", "no")
    )

    if (!.mammAddIsFixedRandom(options, indx))
      resultsS <- resultsS[,colnames(resultsS) != "fixed", drop = FALSE]

    tableS$setData(resultsS)
  }

  ### create summary for the remaining types
  if (fit[["withG"]] && is.null(randomEffectsContainer[["containerG"]])) {

    # create jasp containers
    containerG <- createJaspContainer(title = .mammGetRandomEstimatesTitle(fit[["struct"]][1]))
    containerG$position <- 2
    randomEffectsContainer[["containerG"]] <- containerG
    .mammExtractRandomTables(containerG, options, fit, indx = 1)

  }

  if (fit[["withH"]] && is.null(randomEffectsContainer[["containerH"]])) {

    containerH <- createJaspContainer(title = .mammGetRandomEstimatesTitle(fit[["struct"]][2]))
    containerH$position <- 3
    randomEffectsContainer[["containerH"]] <- containerH
    .mammExtractRandomTables(containerH, options, fit, indx = 2)

  }

  ### create random structure confidence intervals summary
  if (options[["randomEffectsConfidenceIntervals"]] && is.null(randomEffectsContainer[["confidenceIntervalContainers"]]) && !is.null(.mammGetRandomFormulaList(options))) {

    confidenceIntervalsContainer <- createJaspContainer(title = gettext("Confidence Intervals"))
    confidenceIntervalsContainer$position <- 4
    confidenceIntervalsContainer$dependOn(c("randomEffectsConfidenceIntervals", "confidenceIntervalsLevel"))
    randomEffectsContainer[["confidenceIntervalsContainer"]] <- confidenceIntervalsContainer

    # extract precomputed confidence intervals
    confintRandom <- .mammFitConfintRandom(jaspResults, options)[[attr(fit, "subgroup")]]


    # confidence intervals for nested/simple random effects
    if (fit[["withS"]] && is.null(confidenceIntervalsContainer[["confidenceContainerS"]])) {

      confidenceContainerS <- createJaspContainer(title = gettext("Simple / Nested Summary"))
      confidenceContainerS$position <- 1
      confidenceIntervalsContainer[["confidenceContainerS"]] <- confidenceContainerS
      .mammExtractRandomCiTables(confidenceContainerS, options, fit, confintRandom, indx = 0)

    }

    if (fit[["withG"]] && is.null(confidenceIntervalsContainer[["confidenceContainerG"]])) {

      # create jasp containers
      confidenceContainerG <- createJaspContainer(title = .mammGetRandomEstimatesTitle(fit[["struct"]][1]))
      confidenceContainerG$position <- 2
      confidenceIntervalsContainer[["confidenceContainerG"]] <- confidenceContainerG
      .mammExtractRandomCiTables(confidenceContainerG, options, fit, confintRandom, indx = 1)

    }

    if (fit[["withH"]] && is.null(confidenceIntervalsContainer[["confidenceContainerH"]])) {

      # create jasp containers
      confidenceContainerH <- createJaspContainer(title = .mammHetRandomEstimatesTitle(fit[["struct"]][1]))
      confidenceContainerH$position <- 3
      confidenceIntervalsContainer[["confidenceContainerH"]] <- confidenceContainerH
      .mammExtractRandomCiTables(confidenceContainerH, options, fit, confintRandom, indx = 1)

    }
  }


  ### create random structure inclusion summary
  if (options[["randomEffectsTestInclusion"]] && is.null(randomEffectsContainer[["inclusionTestsContainer"]])) {

    inclusionTestsContainer <- createJaspContainer(title = gettext("Inclusion Tests"))
    inclusionTestsContainer$position <- 5
    inclusionTestsContainer$dependOn("randomEffectsTestInclusion")
    randomEffectsContainer[["inclusionTestsContainer"]] <- inclusionTestsContainer

    ### table with general tests for component drop
    tableInclusion <- .mammMakeRandomInclusionTable(title = gettext("Component Inclusion Test"), position = 0)
    inclusionTestsContainer[["tableInclusion"]] <- tableInclusion

    # extract the precomputed drop models
    dropOneFits    <- .mammFitDropOneRandom(jaspResults, options)[[attr(fit, "subgroup")]]

    if (length(dropOneFits) == 0)
      return()

    # compute ANOVAs
    fitTests <- lapply(dropOneFits, function(fitB) data.frame(anova(fit, fitB)))
    fitTests <- rbind(
      cbind(model = "", fitTests[[1]][1,]),
      cbind(model = names(fitTests), do.call(rbind, lapply(fitTests, function(fitTest) fitTest[2,])))
    )

    fitTests <- fitTests[,!colnames(fitTests) %in% "QE"]
    tableInclusion$setData(fitTests)


    ### tables with test for level drop for multilevel components
    if (fit[["withS"]]) {

      # extract the precomputed drop models
      dropLevelFits <- .mammFitDropLevelRandom(jaspResults, options)[[attr(fit, "subgroup")]]

      for (i in seq_along(dropLevelFits)) {

        tempInclusion <- .mammMakeRandomInclusionTable(title = gettextf("%1$s: Level Inclusion Test", names(dropLevelFits)[i]), position = i, removedLevels = TRUE)
        inclusionTestsContainer[[paste0("tableInclusion", i)]] <- tempInclusion

        # level drip design
        levelsGrid    <- attr(dropLevelFits[[i]], "levelsGrid")
        levelsDropped <- sapply(1:nrow(levelsGrid), function(i) paste0(colnames(levelsGrid[!unlist(levelsGrid[i,])]), collapse = ", "))

        # compute ANOVAs
        fitTests <- lapply(dropLevelFits[[i]], function(fitB) data.frame(anova(fit, fitB)))
        fitTests <- rbind(
          cbind(model = "", fitTests[[1]][1,]),
          cbind(model = levelsDropped, do.call(rbind, lapply(fitTests, function(fitTest) fitTest[2,])))
        )

        fitTests <- fitTests[,!colnames(fitTests) %in% "QE"]
        fitTests <- fitTests[order(fitTests$df, decreasing = TRUE),]
        tempInclusion$setData(fitTests)

      }
    }
  }


  return()
}

.mammMakeRandomInclusionTable    <- function(title = gettext("Component Inclusion Test"), position = 0, removedLevels = FALSE) {

  tableInclusion <- createJaspTable(title = title)
  tableInclusion$position <- position

  tableInclusion$addColumnInfo(name = "model",  title = if (removedLevels) gettext("Removed Levels") else gettext("Removed Component"), type = "string")
  tableInclusion$addColumnInfo(name = "logLik", title = gettext("Log Lik."),          type = "number")
  tableInclusion$addColumnInfo(name = "df",     title = gettext("df"),                type = "integer")
  tableInclusion$addColumnInfo(name = "AIC",    title = gettext("AIC"),               type = "number")
  tableInclusion$addColumnInfo(name = "BIC",    title = gettext("BIC"),               type = "number")
  tableInclusion$addColumnInfo(name = "AICc",   title = gettext("AICc"),              type = "number")
  tableInclusion$addColumnInfo(name = "LRT",    title = gettext("LRT"),               type = "number")
  tableInclusion$addColumnInfo(name = "pval",   title = gettext("p"),                 type = "pvalue")

  tableInclusion$addFootnote(gettext("Likelihood Ratio Test (LRT) and p-value are based on a comparison with the complete model."))

  return(tableInclusion)
}

.mammGetRandomEstimatesTitle     <- function(structure) {

  if (structure == "GEN")
    return(gettext("Random Slopes Summary"))
  else if (structure %in% c("CS", "HCS", "UN", "ID", "DIAG"))
    return(paste0(gettext("Structured"), " (", .mammGetOptionsNameStructure(structure), ") ", gettext("Summary")))
  else if (structure %in% c("AR", "HAR", "CAR"))
    return(paste0(gettext("Autoregressive"), " (", .mammGetOptionsNameStructure(structure), ") ", gettext("Summary")))
  else if (structure %in% c("SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH"))
    return(paste0(gettext("Spatial"), " (", .mammGetOptionsNameStructure(structure), ") ", gettext("Summary")))
  else
    return(gettext("Known Correlation Summary"))
}

# Populate tempContainer from a subgroup or full-fit random-effects result.
# indx selects the G (1) or H (2) variance/correlation component in x.
.mammExtractRandomTables         <- function(tempContainer, options, x, indx = 1) {

  # dispatching
  struct <- x$struct[indx]

  tau2 <- if (indx == 1) x[["tau2"]] else x[["gamma2"]]
  tau  <- sqrt(tau2)
  rho  <- if (indx == 1) x[["rho"]] else x[["phi"]]

  tau2Name <- if (indx == 1) "tau2" else "gamma2"
  rhoName  <- if (indx == 1) "rho"  else "phi"

  GName               <- if (indx == 1) "G" else "H"
  g.levels.kName      <- if (indx == 1) "g.levels.k" else "h.levels.k"
  g.levels.fName      <- if (indx == 1) "g.levels.f" else "h.levels.f"
  g.nlevels.kName     <- if (indx == 1) "g.nlevels.k" else "h.nlevels.k"
  g.nlevels.fName     <- if (indx == 1) "g.nlevels.f" else "h.nlevels.f"
  g.levels.comb.kName <- if (indx == 1) "g.levels.comb.k" else "h.levels.comb.k"
  g.nlevelsName       <- if (indx == 1) "g.nlevels" else "h.nlevels"
  g.namesName         <- if (indx == 1) "g.names" else "h.names"


  # create information messages
  if (is.element(struct, c("SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD", "GEN", "GDIAG"))) {
    inner <- trimws(paste0(strsplit(paste0(x$formulas[[indx]], collapse = ""), "|", fixed = TRUE)[[1]][1], collapse = ""))
  } else {
    inner <- x[[g.namesName]][1]
  }
  outer    <- tail(x[[g.namesName]], 1)
  innerLvl <- x[[g.nlevels.fName]][1]
  outerLvl <- x[[g.nlevelsName]][2]

  message1 <- paste0(x[[g.nlevels.fName]][1], " | ", outerLvl)
  message2 <- paste0(inner, " | ", outer)

  if (is.element(struct, c("CS", "AR", "CAR", "ID", "SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD"))) {

    vc <- cbind(tau2, tau, ifelse(x$vc.fix[[tau2Name]], "yes", "no"))
    vc <- rbind(vc, c(rho, "", ifelse(x$vc.fix[[rhoName]], "yes", "no")))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "fixed")
    vc$parameter <- c("\U1D70F\U00B2", "\U03C1")
    for(colName in c("estimate", "estimateSqrt")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (struct == "ID") {
      vc <- vc[1, , drop = FALSE]
    }

   if (!.mammAddIsFixedRandom(options, indx))
     vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))
    tempContainer[["table1"]] <- tempTable

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))

    }

  if (is.element(struct, c("HCS", "HAR", "DIAG"))) {

    vc <- cbind(tau2, tau, x[[g.levels.kName]], ifelse(x$vc.fix$tau2, "yes", "no"), x[[g.levels.fName]][[1]])
    vc <- rbind(vc, c(rho, "", "", ifelse(x$vc.fix[[rhoName]], "yes", "no"), ""))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "nLevels", "fixed", "level")
    for(colName in c("estimate", "estimateSqrt", "nLevels")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (length(x[[tau2Name]]) == 1L) {
      vc$parameter <- c("\U1D70F\U00B2", "\U03C1")
    } else {
      vc$parameter <- c(paste0("\U1D70F\U00B2[",seq_along(x[[tau2Name]]),"]"), "\U03C1")
    }

    if (struct == "DIAG")
      vc <- vc[seq_along(tau2), , drop = FALSE]

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "level",          type = "string",  title = gettext("Level"))
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    tempTable$addColumnInfo(name = "nLevels",        type = "integer", title = gettext("Levels"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable

  }

  if (is.element(struct, c("UN", "UNR"))) {

    if (struct == "UN") {
      vc <- cbind(tau2, tau, x[[g.levels.kName]], ifelse(x$vc.fix[[tau2Name]], "yes", "no"), x[[g.levels.fName]][[1]])
    } else {
      vc <- cbind(rep(tau2, length(x[[g.levels.kName]])),
                  rep(tau, length(x[[g.levels.kName]])), x[[g.levels.kName]],
                  ifelse(rep(x$vc.fix[[tau2Name]], length(x[[g.levels.kName]])), "yes", "no"),
                  x[[g.levels.fName]][[1]])
    }
    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "nLevels", "fixed", "level")
    for(colName in c("estimate", "estimateSqrt", "nLevels")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (length(x[[g.levels.kName]]) == 1L) {
      vc$parameter <- c("\U1D70F\U00B2")
    } else {
      vc$parameter <-paste0("\U1D70F\U00B2[",seq_along(x[[g.levels.kName]]),"]")
    }

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates \U1D70F\U00B2"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "level",          type = "string",  title = gettext("Level"))
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    tempTable$addColumnInfo(name = "nLevels",        type = "string",  title = gettext("Levels"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable


    if (length(x[[rhoName]]) == 1L) {
      G <- matrix(NA_real_, nrow = 2, ncol = 2)
    } else {
      G <- matrix(NA_real_, nrow = x[[g.nlevels.fName]][1], ncol = x[[g.nlevels.fName]][1])
    }

    G[lower.tri(G)] <- rho
    G[upper.tri(G)] <- t(G)[upper.tri(G)]
    diag(G) <- 1
    G[upper.tri(G)] <- NA

    G <- data.frame(G)
    colnames(G) <- paste0("rho", 1:(ncol(G)))
    G$parameter <- sprintf("\U03C1[%1$i,]", 1:nrow(G))


    if (length(x$rho) == 1L) {
      G.info <- matrix(NA_real_, nrow = 2, ncol = 2)
    } else {
      G.info <- matrix(NA_real_, nrow = x[[g.nlevels.fName]][1], ncol = x[[g.nlevels.fName]][1])
    }
    G.infoLevels <- G.info
    G.infoLevels[lower.tri(G.infoLevels)] <- x[[g.levels.comb.kName]]
    G.infoLevels[upper.tri(G.infoLevels)] <- t(G.infoLevels)[upper.tri(G.infoLevels)]
    G.infoLevels[lower.tri(G.infoLevels)] <- NA
    diag(G.infoLevels) <- NA

    G.infoLevels <- data.frame(G.infoLevels)
    colnames(G.infoLevels) <- paste0("rhoLevel", 1:ncol(G.infoLevels))

    G.infoEstimated <- G.info
    G.infoEstimated[upper.tri(G.infoEstimated)] <- ifelse(x$vc.fix[[rhoName]], "yes", "no")

    G.infoEstimated <- data.frame(G.infoEstimated)
    colnames(G.infoEstimated) <- paste0("rhoEstimated", 1:ncol(G.infoEstimated))

    if (!.mammAddIsFixedRandom(options, indx))
      Gmat <- cbind(G, G.infoLevels)
    else
      Gmat <- cbind(G, G.infoLevels, G.infoEstimated)

    tempTable2 <- createJaspTable(title = gettext("Estimates \U03C1"))
    tempTable2$position <- 2
    tempTable2$addColumnInfo(name = "parameter", type = "string",  title = "")
    for(i in 1:(ncol(G)-1)){
      tempTable2$addColumnInfo(name = paste0("rho",i), type = "number", title = sprintf("[,%1$i]", i), overtitle = gettext("Estimates"))
    }
    for(i in 1:ncol(G.infoLevels)){
      tempTable2$addColumnInfo(name = paste0("rhoLevel",i), type = "integer", title = sprintf("[,%1$i]", i), overtitle = gettext("Levels"))
    }
    if (.mammAddIsFixedRandom(options, indx)) {
      for(i in 1:ncol(G.infoEstimated)){
        tempTable2$addColumnInfo(name = paste0("rhoEstimated",i), type = "string", title = sprintf("[,%1$i]", i), overtitle = gettext("Fixed"))
      }
    }

    tempTable2$setData(Gmat)
    tempTable2$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable2$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table2"]] <- tempTable2
  }

  if (is.element(struct, c("GEN"))) {

    vc <- cbind(tau2, tau, ifelse(x$vc.fix[[tau2Name]], "yes", "no"))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "fixed")
    vc$parameter <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))
    for(colName in c("estimate", "estimateSqrt")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates \U1D70F\U00B2"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable


    G.info <- cov2cor(x[[GName]])
    diag(G.info) <- NA
    G.info[upper.tri(G.info)] <- NA

    G.info <- data.frame(G.info)
    colnames(G.info) <- paste0("rho", 1:ncol(G.info))


    G.infoFixed <- G.info
    G.infoFixed[lower.tri(G.infoFixed)] <- NA
    G.infoFixed[upper.tri(G.infoFixed)] <- ifelse(x$vc.fix[[rhoName]], "yes", "no")

    G.infoFixed <- data.frame(G.infoFixed)
    colnames(G.infoFixed) <- paste0("rhoFixed", 1:ncol(G.infoFixed))

    if (!.mammAddIsFixedRandom(options, indx))
      Gmat <- G.info
    else
      Gmat <- cbind(G.info, G.infoFixed)

    Gmat$parameter <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))


    tempTable2 <- createJaspTable(title = gettext("Estimates \U03C1"))
    tempTable2$position <- 2
    tempTable2$addColumnInfo(name = "parameter", type = "string",  title = "")
    for(i in 1:ncol(G.info)){
      tempTable2$addColumnInfo(name = paste0("rho",i), type = "number", title = Gmat$parameter[i], overtitle = gettext("Estimates"))
    }
    if (.mammAddIsFixedRandom(options, indx)) {
      for(i in 1:ncol(G.infoFixed)){
        tempTable2$addColumnInfo(name = paste0("rhoFixed",i), type = "string", title = Gmat$parameter[i], overtitle = gettext("Fixed"))
      }
    }

    tempTable2$setData(Gmat)
    tempTable2$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable2$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table2"]] <- tempTable2
  }

  if (is.element(struct, c("GDIAG"))) {

    vc <- cbind(tau2, tau, ifelse(x$vc.fix[["tau2"]], "yes", "no"))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "fixed")
    vc$parameter <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))
    for(colName in c("estimate", "estimateSqrt")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates \U1D70F\U00B2"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))


    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable
  }

  return()
}

.mammExtractRandomCiTables       <- function(tempContainer, options, x, confintRandom, indx = 0) {

  overtitleCi <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])

  tau2Name <- if (indx == 0) "sigma2" else if (indx == 1) "tau2" else "gamma2"
  tauName  <- if (indx == 0) "sigma"  else if (indx == 1) "tau"  else "gamma"
  rhoName  <- if (indx == 0) NA       else if (indx == 1) "rho"  else "phi"
  struct   <- if (indx == 0) "simple" else if (indx == 1) x$struct[indx]

  GName               <- if (indx == 1) "G" else "H"
  g.levels.kName      <- if (indx == 1) "g.levels.k" else "h.levels.k"
  g.levels.fName      <- if (indx == 1) "g.levels.f" else "h.levels.f"
  g.nlevels.kName     <- if (indx == 1) "g.nlevels.k" else "h.nlevels.k"
  g.nlevels.fName     <- if (indx == 1) "g.nlevels.f" else "h.nlevels.f"
  g.levels.comb.kName <- if (indx == 1) "g.levels.comb.k" else "h.levels.comb.k"
  g.nlevelsName       <- if (indx == 1) "g.nlevels" else "h.nlevels"
  g.namesName         <- if (indx == 1) "g.names" else "h.names"

  if (struct == "simple") {
    title1 <- gettext("Estimates")
  } else {
    title1 <- gettext("Estimates \U1D70F\U00B2")
  }

  tempTable <- createJaspTable(title1)
  tempTable$position <- 1

  tempTable$addColumnInfo(name = "par",       type = "string", title = "")
  tempTable$addColumnInfo(name = "estTau2", type = "number", title = if (indx == 0) gettext("\U03C3\U00B2") else gettext("\U1D70F\U00B2"))
  tempTable$addColumnInfo(name = "lCiTau2", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
  tempTable$addColumnInfo(name = "uCiTau2", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  tempTable$addColumnInfo(name = "estTau",  type = "number", title = if (indx == 0) gettext("\U03C3") else gettext("\U1D70F"))
  tempTable$addColumnInfo(name = "lCiTau",  title = gettext("Lower"), type = "number", overtitle = overtitleCi)
  tempTable$addColumnInfo(name = "uCiTau",  title = gettext("Upper"), type = "number", overtitle = overtitleCi)

  # extract the estimates
  tauCi  <- confintRandom[grepl(tauName, confintRandom$parameter) & !grepl(paste0(tauName,"^2"), confintRandom$parameter, fixed = TRUE),,drop=FALSE]
  tau2Ci <- confintRandom[grepl(paste0(tauName,"^2"), confintRandom$parameter, fixed = TRUE),,drop=FALSE]

  # create parameter names
  if (struct == "simple") {
    par1Levels <- .maVariableNames(x[["s.names"]], unlist(.mammExtractRandomVariableNames(options)))
  } else if (is.element(struct, c("CS", "AR", "CAR", "ID", "SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD"))) {
    par1Levels <- "\U1D70F\U00B2"
  } else if (is.element(struct, c("HCS", "HAR", "DIAG"))) {
    if (length(x[[tau2Name]]) == 1L) {
      par1Levels <- "\U1D70F\U00B2"
    } else {
      par1Levels <- c(paste0("\U1D70F\U00B2[",seq_along(x[[tau2Name]]),"]"))
    }
  } else if (is.element(struct, c("UN", "UNR"))) {
    if (length(x[[g.levels.kName]]) == 1L) {
      par1Levels <- c("\U1D70F\U00B2")
    } else {
      par1Levels <- paste0("\U1D70F\U00B2[",seq_along(x[[g.levels.kName]]),"]")
    }
  } else if (is.element(struct, c("GEN", "GDIAG"))) {
    par1Levels <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))
  }

  tempData <- data.frame(
    par     = par1Levels,
    estTau2 = tau2Ci$estimate,
    lCiTau2 = tau2Ci$ci.lb,
    uCiTau2 = tau2Ci$ci.ub,
    estTau  = tauCi$estimate,
    lCiTau  = tauCi$ci.lb,
    uCiTau  = tauCi$ci.ub
  )
  tempTable$setData(tempData)
  tempContainer[["tempTable"]] <- tempTable

  # some structures have only one parameter
  if (is.element(struct, c("simple", "DIAG", "GDIAG", "ID"))) {
    return()
  }

  tempTable2 <- createJaspTable(title1)
  tempTable2$position <- 1

  tempTable2$addColumnInfo(name = "par", type = "string", title = "")
  tempTable2$addColumnInfo(name = "est", type = "number", title = gettext("\U03C1"))
  tempTable2$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
  tempTable2$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)

  # extract the estimates
  rhoCi  <- confintRandom[grepl(rhoName, confintRandom$parameter),,drop=FALSE]

  # create parameter names
  if (is.element(struct, c("CS", "AR", "CAR", "ID", "SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD", "HCS", "HAR", "DIAG"))) {
    par2Levels <- "\U03C1"
  } else if (is.element(struct, c("UN", "UNR"))) {
    if (length(x[[rhoName]]) == 1L) {
      par2Levels <- "\U03C1[2,1]"
    } else {
      par2Levels <- NULL
      for (i in 1:x[[g.nlevels.fName]][1]) {
        for (j in 1:x[[g.nlevels.fName]][1]) {
          if (i < j)
            par2Levels <- c(par2Levels, paste0("\U03C1[", j, ",", i, "]"))
        }
      }
    }
  } else if (is.element(struct, c("GEN"))) {
    par2Levels    <- NULL
    par2Variables <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))
    for (i in 1:length(par2Variables)) {
      for (j in 1:length(par2Variables)) {
        if (i < j)
          par2Levels <- c(par2Levels, paste0("\U03C1[", par2Variables[j], ",", par2Variables[i], "]"))
      }
    }
  }

  tempData2 <- data.frame(
    par = par2Levels,
    est = rhoCi$estimate,
    lCi = rhoCi$ci.lb,
    uCi = rhoCi$ci.ub
  )
  tempTable2$setData(tempData2)
  tempContainer[["tempTable2"]] <- tempTable2

  return()
}

.mammAddIsFixedRandom            <- function(options, indx) {

  return(FALSE)

  # TODO: show / hide information on whether the random effects are fixed by the user
}

.mammConfintIterations           <- function(x) {

  iterations <- 0
  if(x$withS && any(!x$vc.fix$sigma2))
    iterations <- iterations + length(seq_len(x$sigma2s)[!x$vc.fix$sigma2])

  if (x$withG) {
    if (any(!x$vc.fix$tau2))
      iterations <- iterations + length(seq_len(x$tau2s)[!x$vc.fix$tau2])

    if (any(!x$vc.fix$rho))
      iterations <- iterations + length(seq_len(x$rhos)[!x$vc.fix$rho])
  }

  if (x$withH) {
    if (any(!x$vc.fix$gamma2))
      iterations <- iterations + length(seq_len(x$gamma2s)[!x$vc.fix$gamma2])

    if (any(!x$vc.fix$phi))
      iterations <- iterations + length(seq_len(x$phis)[!x$vc.fix$phi])
  }

  return(iterations)
}
