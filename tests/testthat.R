library(jaspTools)
library(testthat)

install.packages("RoBMA")

jaspTools::runTestsTravis(module = getwd())
