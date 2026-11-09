context("Bayesian Meta Analysis (Deprecated)")

options <- jaspTools::analysisOptions("BayesianMetaAnalysisDeprecated")
options$effectSize                              <- "yi"
options$effectSizeSe                            <- "sei"
options$modelProbability                        <- TRUE
options$effectSizePerStudy                      <- TRUE
options$priorPlot                               <-  TRUE
options$priorPosterior                          <- TRUE
options$forestPlot                              <- TRUE
options$cumulativeForestPlot                    <- FALSE
options$bfSequentialPlot                        <- FALSE
options$modelProbabilitySequentialPlot          <- FALSE
options$priorPosteriorAdditionalInfo            <- TRUE
options$priorPosteriorFixedAndRandom            <- TRUE
options$priorPosteriorCi                        <- TRUE
options$priorModelProbabilityFixedNull          <- 0.25
options$priorModelProbabilityFixedAlternative   <- 0.25
options$priorModelProbabilityRandomNull         <- 0.25
options$priorModelProbabilityRandomAlternative  <- 0.25
options$samples                                 <- 2000
options$priorEffectSize                         <- "cauchy"
options$priorStandardError                      <- "inverseGamma"
options$forestPlotEffect                        <- "observed"
options$forestPlotRowOrder                      <- "ascending"
options$bayesFactorComputation                  <- "integration"
options$bayesFactorType                         <- "BF10"
options$model                                   <- "averaging"
options$truncationLowerBound                    <- FALSE
options$truncationUpperBound                    <- FALSE
options$cauchyLocation                          <- 0
options$cauchyScale                             <- 0.707
options$inverseGammaShape                       <- 1
options$inverseGammaScale                       <- 0.15
options$chains                                  <- 4
options$seed                                    <- 1
options$.meta <- list(effectSizeCi = list(containsColumn = TRUE),
                      effectSize = list(containsColumn = TRUE),
                      effectSizeSe = list(containsColumn = TRUE),
                      studyLabel = list(containsColumn = TRUE))
options$effectSizeCi <- list()
set.seed(1)

dat <- data.frame(metafor::escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=metadat::dat.bcg, slab=paste0(author, ", ", year)))
dat$sei <- sqrt(dat$vi)

results <- jaspTools::runAnalysis("BayesianMetaAnalysisDeprecated", dataset = dat, options)

############################ NOTE ############################
# unit tests currently only work for Windows due to rstan seed
# when they start failing it might be due to seed issues
# you can skip the tests if this is the case
##############################################################

test_that("Posterior Estimates per Model table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 1.08511272548537e+23, -0.427502612605115, 0.0415313437232907,
                                      -0.507586496309618, "Fixed effects", "<unicode>", -0.346356207511908,
                                      "TRUE", 43.7889383833054, -0.661933042053905, 0.191320159992726,
                                      -1.02793122597389, "Random effects", "<unicode>", -0.272973761670359,
                                      1, "FALSE", 4.17330245755268e+24, 0.566106388852281, 0.164755335303674,
                                      0.333044419541003, "Random effects", "<unicode>", 0.979316290250099,
                                      2, "TRUE", 43.7889383833053, -0.661933086283698, 0.191254840143733,
                                      -1.03550532527557, "Averaged", "<unicode>", -0.271096143741007,
                                      3, "FALSE", 4.26860740468391e+24, "", "", "", "Averaged", "<unicode>",
                                      ""))
})

test_that("Effect Sizes per Study table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["effectSizePerStudy"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.76219528736861, -1.56463441301078, 0.00434120986466792, -0.889311333920205,
                                      "Study 1", -1.21641414555462, -1.95241736326061, -0.531409107211549,
                                      -1.58538865720143, "Study 2", -0.954517427695111, -1.87034134818823,
                                      -0.154296468042579, -1.34807314829969, "Study 3", -1.38732597041773,
                                      -1.65142083964601, -1.11738294707035, -1.44155119002131, "Study 4",
                                      -0.287787276512124, -0.700522547753564, 0.138136667222637, -0.217547322211296,
                                      "Study 5", -0.783517031439593, -0.950629570696096, -0.616959899178507,
                                      -0.786115585818864, "Study 6", -1.21034016392422, -1.96329934619858,
                                      -0.508281572004106, -1.62089822359839, "Study 7", 0.00137143265956823,
                                      -0.120012114156573, 0.123694374821191, 0.0119523335238405, "Study 8",
                                      -0.502548672291503, -0.945222651201392, -0.0680976207100825,
                                      -0.469417648738149, "Study 9", -1.22422540793158, -1.7220073712744,
                                      -0.738690338866749, -1.37134480347278, "Study 10", -0.354609964569658,
                                      -0.567400785455011, -0.141918131484038, -0.339358828338391,
                                      "Study 11", -0.258713651668148, -1.14521916184458, 0.705133634656889,
                                      0.445913400571379, "Study 12", -0.153423794460656, -0.645389334770901,
                                      0.335881268730614, -0.0173139482168798, "Study 13"))
})

test_that("Observed study effects plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlotEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Model Probabilities table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["modelProbability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Fixed H<unicode>", 2.15893183301841e-48, 0.25, "Fixed H<unicode>",
                                      2.34268440546373e-25, 0.25, "Random H<unicode>", 0.0223269413407829,
                                      0.25, "Random H<unicode>", 0.977673058659217, 0.25))
})

test_that("Effect size plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Effect Size plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})
