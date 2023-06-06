# Set renv profile base on R version.
if ((Sys.getenv("GITHUB_ACTIONS") == "") && (Sys.getenv("DOCKER_CONTAINER_CONTEXT") == "")) {
  renv_profile <- paste(R.version$major, substr(R.version$minor, 1, 1), sep = ".")
  if (file.exists("./renv/profile")) {
    message("Using renv profile from `renv/profile` file.")
  } else if (renv_profile %in% c("4.1", "4.2", "4.3")) {
    message("Set renv profile to `", renv_profile, "`")
    Sys.setenv("RENV_PROFILE" = renv_profile)
  } else {
    message("This repository do not contains the renv profile for your R version.")
  }
  source("renv/activate.R")
} else {
  options(repos = c(CRAN = "https://cran.rstudio.com"))
}
