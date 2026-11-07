## Launches the registry editor for local development.
## Run with `Rscript app_registry_editor.R`, or open this file in
## RStudio and click "Run App".
##
## Points at the registry shipped in inst/registry by default -- that
## means running this edits that file (and its git-tracked binary) in
## place. Copy it elsewhere first (and change registry_path below) if
## you want to try edits without touching the package's own demo data.

devtools::load_all(".", quiet = TRUE)

registry_path <- system.file("registry/registry.duckdb", package = "proviz")
run_registry_editor(registry_path)
