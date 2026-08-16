
########################
### Load Packages ######
########################

# renv allows us to record the exact package versions used by the project

# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

#renv::snapshot()
#renv::restore()

renv::snapshot()
rsconnect::writeManifest()

#######################
#### Download Data ####
#######################

source("R/1.1_DownloadData_PAK.R")



