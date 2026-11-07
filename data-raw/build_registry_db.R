## ---------------------------------------------------------------------------
## data-raw/build_registry_db.R
##
## (Re)generates the registry.duckdb + synthetic log CSVs + adoption.xlsx
## shipped in inst/registry/, by calling the same generator pv_demo_registry()
## uses for ad-hoc demo data (see R/pv_demo.R), just pointed at inst/registry/
## instead of a temp directory.
##
## Re-run this script (Rscript data-raw/build_registry_db.R from the package
## root) only to regenerate the shipped reference/demo registry.
## ---------------------------------------------------------------------------

devtools::load_all(".", quiet = TRUE)

out_dir <- "inst/registry"
unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE)

reg <- pv_demo_registry(dir = out_dir, seed = 42)
cat("Wrote", out_dir, "--", nrow(reg$processes), "processes,", nrow(reg$subprocesses), "subprocesses.\n")
