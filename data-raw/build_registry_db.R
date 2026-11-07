## ---------------------------------------------------------------------------
## data-raw/build_registry_db.R
##
## Builds the shipped inst/registry/registry.duckdb from the original
## hand-authored YAML registry, preserved under data-raw/legacy_registry_yaml/
## for this conversion's sake (and as a historical record) -- it is no longer
## read at runtime; pv_load_registry() reads the DuckDB file directly.
##
## Re-run this script (Rscript data-raw/build_registry_db.R from the package
## root) only to regenerate the shipped demo/reference registry from that
## legacy YAML. A real organization's registry is built once with
## pv_registry_migrate_yaml() and from then on edited in place (eventually
## via the companion registry editor app), not regenerated from YAML.
## ---------------------------------------------------------------------------

devtools::load_all(".", quiet = TRUE)

yaml_path <- "data-raw/legacy_registry_yaml/processes.yml"
db_path <- "inst/registry/registry.duckdb"

if (file.exists(db_path)) file.remove(db_path)
pv_registry_migrate_yaml(yaml_path, db_path)

reg <- pv_load_registry(db_path)
cat("Wrote", db_path, "--", nrow(reg$processes), "processes,", nrow(reg$subprocesses), "subprocesses,",
  length(reg$ideal), "ideal-process entries.\n")
