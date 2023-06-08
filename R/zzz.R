.onLoad <- function(libname, pkgname) {

  if (jaspBase:::getOS() == "osx" &&
      isTRUE(try(jaspBase::jaspResultsCalledFromJasp()))
  ) {

    jagsHome <- Sys.getenv("JAGS_HOME")
    options(jags.moddir = file.path(jagsHome, "modules-4"))
    runjags::runjags.options(jagspath = jagsHome)

  }
}
