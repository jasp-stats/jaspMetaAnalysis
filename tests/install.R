repos <- getOption("repos")
repos["CRAN"] <- "https://cloud.r-project.org"
options(repos = repos)
options(pkgType="binary")

install.packages("remotes")

jaspResultsPath <- file.path("~", "jasp-desktop", "JASP-R-Interface", "jaspResults")
remotes::install_deps(jaspResultsPath, upgrade=FALSE)
install.packages(jaspResultsPath, type="source", repos=NULL)

jaspGraphsPath <- file.path("~", "jasp-desktop", "JASP-Engine", "jaspGraphs")
remotes::install_deps(jaspGraphsPath, upgrade=FALSE)
install.packages(jaspGraphsPath, type="source", repos=NULL)

JASPPath <- file.path("~", "jasp-desktop", "JASP-Engine", "jaspBase")
# remotes::install_deps(JASPPath, upgrade=FALSE) # JASP doesn't have any dependencies (yet)
install.packages(JASPPath, type="source", repos=NULL)

jasptoolsPath <- file.path("~", "jasp-desktop", "Tools", "jaspTools")
remotes::install_deps(jasptoolsPath, upgrade=FALSE)
install.packages(jasptoolsPath, type="source", repos=NULL)
