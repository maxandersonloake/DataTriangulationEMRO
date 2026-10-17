# ================================================================
# Record the project's package versions and refresh the Posit Connect Cloud
# deployment manifest.
# ----------------------------------------------------------------
# Run from the project root after installing, updating or removing a
# package, or after adding/renaming/deleting any deployed file:
#   Rscript scripts/update_renv_manifest.R
#
# renv::snapshot() rewrites renv.lock; rsconnect::writeManifest() rewrites
# manifest.json (the file list and package versions Posit Connect Cloud
# reads when the app is published).
# ================================================================

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::snapshot()
rsconnect::writeManifest()
