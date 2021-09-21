.onLoad <- function(libname, pkgname) {

  # function from zzzWrappers.R inside jaspResults
  jaspResultsCalledFromJasp <- mget("jaspResultsCalledFromJasp", envir = .GlobalEnv, mode = "function", ifnotfound = NA)

  if (!is.na(jaspResultsCalledFromJasp) &&
      jaspBase:::getOS() == "osx" &&
      isTRUE(try(jaspResultsCalledFromJasp()))
  ) {

    jagsHome <- Sys.getenv("JAGS_HOME")
    options(jags.moddir = file.path(jagsHome, "modules-4"))
    runjags::runjags.options(jagspath = jagsHome)

  }
}
