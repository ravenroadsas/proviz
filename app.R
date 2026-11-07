## Launches the process intelligence dashboard for local development.
## Run with `Rscript app.R`, or open this file in RStudio and click
## "Run App".
##
## Defaults to the synthetic demo registry (see pv_demo_registry()) so
## the dashboard works with no real shared mount or application data.
## To point it at a real registry instead, set demo = FALSE and pass
## registry_path (defaults to the registry shipped in inst/registry).

devtools::load_all(".", quiet = TRUE)

run_pi_dashboard(demo = TRUE)
