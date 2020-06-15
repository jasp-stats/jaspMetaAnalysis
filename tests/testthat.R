library(jaspResults)
library(jasptools)
library(testthat)
develop(path = file.path("~", "jasp-desktop"))
setPkgOption("pkgs.dir", pkgsDir)

options("testthat.progress.max_fails" = 1E3L)

result <- test_dir("testthat")
result <- as.data.frame(result)

if (sum(result$failed) > 0 || sum(result$error) > 0)
  quit(save = "no", status = 1)