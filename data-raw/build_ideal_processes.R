## ---------------------------------------------------------------------------
## data-raw/build_ideal_processes.R
##
## One-off generator for inst/registry/ideal_processes/*.yml, migrating the
## two ideal processes modeled in the original PoC (ravenroadsas/tests,
## R/01_ideal_process.R and R/02_type_well_process.R) into the
## activities+variants YAML shape pv_load_ideal_process() reads.
##
## Re-run this script (Rscript data-raw/build_ideal_processes.R from the
## package root) only if the ideal-process definitions themselves change --
## the generated YAML is what ships in inst/, this script is not part of the
## package.
## ---------------------------------------------------------------------------

stopifnot(requireNamespace("yaml", quietly = TRUE))
out_dir <- "inst/registry/ideal_processes"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## -----------------------------------------------------------------------
## Process 1: daily production surveillance (branch-heavy, no loops).
## The PoC's build_case_sequence() enumerates every branch combination as
## an equally acceptable path -- there is no single "happy path" here, so
## all combinatorial variants are declared ideal.
## -----------------------------------------------------------------------

surveillance_activities <- list(
  list(id = "Data_Ingestion",           label = "Pull SCADA & Historian Data",                 role = "Surveillance Analyst"),
  list(id = "Data_Quality_Check",       label = "Validate & Cleanse Meter Readings",           role = "Surveillance Analyst"),
  list(id = "Production_Allocation",    label = "Reconcile Well-to-Facility Allocation",       role = "Production Engineer"),
  list(id = "Variance_Analysis",        label = "Compare Actual vs Forecast/Nomination",       role = "Production Engineer"),
  list(id = "Well_Anomaly_Screening",   label = "Screen Wells for Anomalies",                  role = "Production Engineer"),
  list(id = "Artificial_Lift_Review",   label = "Review Artificial Lift Performance",          role = "Reliability Engineer"),
  list(id = "Exception_Prioritization", label = "Prioritize Flagged Deviations",               role = "Production Engineer"),
  list(id = "Root_Cause_Diagnostics",   label = "Diagnose Root Cause on Flagged Wells",        role = "Reliability Engineer"),
  list(id = "Forecast_Update",          label = "Update Production Forecast / Decline Curve",  role = "Production Engineer"),
  list(id = "Surveillance_Reporting",   label = "Log Findings in Daily Surveillance Report",   role = "Surveillance Analyst"),
  list(id = "Issue_Escalation",         label = "Escalate Critical Issue to Ops/Engineering",  role = "Shift Supervisor"),
  list(id = "Report_Signoff",           label = "Review & Sign Off Daily Report",              role = "Shift Supervisor")
)

## Mirrors build_case_sequence() in tests/R/01_ideal_process.R exactly.
build_surveillance_sequence <- function(has_lift, has_exception, forecast_update, critical) {
  seq_ids <- c("Data_Ingestion", "Data_Quality_Check", "Production_Allocation",
               "Variance_Analysis", "Well_Anomaly_Screening")
  if (has_lift) seq_ids <- c(seq_ids, "Artificial_Lift_Review")
  seq_ids <- c(seq_ids, "Exception_Prioritization")
  if (has_exception) {
    seq_ids <- c(seq_ids, "Root_Cause_Diagnostics")
    if (forecast_update) seq_ids <- c(seq_ids, "Forecast_Update")
  }
  seq_ids <- c(seq_ids, "Surveillance_Reporting")
  if (has_exception && critical) seq_ids <- c(seq_ids, "Issue_Escalation")
  seq_ids <- c(seq_ids, "Report_Signoff")
  seq_ids
}

surveillance_variants <- list()
for (has_lift in c(FALSE, TRUE)) {
  for (has_exception in c(FALSE, TRUE)) {
    fu_opts <- if (has_exception) c(FALSE, TRUE) else FALSE
    cr_opts <- if (has_exception) c(FALSE, TRUE) else FALSE
    for (forecast_update in fu_opts) {
      for (critical in cr_opts) {
        surveillance_variants[[length(surveillance_variants) + 1]] <-
          build_surveillance_sequence(has_lift, has_exception, forecast_update, critical)
      }
    }
  }
}
surveillance_variants <- unique(surveillance_variants)

yaml::write_yaml(
  list(
    process_id = "daily_production_surveillance",
    activities = surveillance_activities,
    variants = surveillance_variants
  ),
  file.path(out_dir, "daily_production_surveillance.yml")
)

## -----------------------------------------------------------------------
## Process 2: type curve construction (loop/lifecycle-heavy). The PoC's
## generate_case() allows 0-3 rework passes on two independent loops plus
## an open-ended review/rejection cycle -- exhaustively enumerating every
## loop count is not a meaningful "ideal" set (that's exactly why v1 scores
## conformance by edit distance rather than exact match). Declaring a small
## set of representative variants instead: this is an illustrative choice
## for demo data, not a substitute for the domain SME sign-off the playbook
## calls for in Step 1 when onboarding a real process.
## -----------------------------------------------------------------------

type_curve_activities <- list(
  list(id = "Define_Population_Criteria", label = "Define Type Well Population Criteria", role = "Reservoir Engineer"),
  list(id = "Query_Analog_Wells",         label = "Query Candidate Analog Wells",          role = "Data Analyst"),
  list(id = "Review_Analog_Statistics",   label = "Review Analog Set Statistics",          role = "Reservoir Engineer"),
  list(id = "Refine_Analog_Selection",    label = "Refine Analog Well Selection",          role = "Reservoir Engineer"),
  list(id = "Normalize_Production_Data",  label = "Normalize Analog Production Data",      role = "Data Analyst"),
  list(id = "Generate_Aggregate_Curve",   label = "Generate Statistical Aggregate Curve",  role = "Reservoir Engineer"),
  list(id = "Fit_Decline_Curve",          label = "Fit Decline Curve Model",               role = "Reservoir Engineer"),
  list(id = "Adjust_Fitted_Curve",        label = "Manually Adjust Fitted Curve",          role = "Reservoir Engineer"),
  list(id = "Compare_Existing_Curves",    label = "Compare Against Existing Type Curves",  role = "Reservoir Engineer"),
  list(id = "Save_Draft",                 label = "Save / Export Type Curve (Draft)",      role = "Reservoir Engineer"),
  list(id = "Postulate_For_Review",       label = "Postulate Draft for Review",            role = "Reservoir Engineer"),
  list(id = "Super_User_Review",          label = "Super User Review",                     role = "Super User"),
  list(id = "Publish_Official",           label = "Publish to Type Well Library",          role = "Super User")
)

base_front <- c("Define_Population_Criteria", "Query_Analog_Wells", "Review_Analog_Statistics")
base_back  <- c("Normalize_Production_Data", "Generate_Aggregate_Curve", "Fit_Decline_Curve",
                "Compare_Existing_Curves", "Save_Draft")
submit_approve <- c("Postulate_For_Review", "Super_User_Review", "Publish_Official")

type_curve_variants <- list(
  ## Straight-through: no rework, approved first pass.
  c(base_front, base_back, submit_approve),
  ## One analog-refinement loop before proceeding.
  c(base_front, "Refine_Analog_Selection", "Review_Analog_Statistics", base_back, submit_approve),
  ## One manual curve-adjustment pass.
  c(base_front, "Normalize_Production_Data", "Generate_Aggregate_Curve", "Fit_Decline_Curve",
    "Adjust_Fitted_Curve", "Compare_Existing_Curves", "Save_Draft", submit_approve),
  ## One rejection -> curve rework -> resubmit -> approved cycle.
  c(base_front, base_back, "Postulate_For_Review", "Super_User_Review",
    "Adjust_Fitted_Curve", "Save_Draft", submit_approve)
)

yaml::write_yaml(
  list(
    process_id = "type_curve_construction",
    activities = type_curve_activities,
    variants = type_curve_variants
  ),
  file.path(out_dir, "type_curve_construction.yml")
)

cat("Wrote", length(surveillance_variants), "surveillance variants and",
    length(type_curve_variants), "type-curve variants.\n")
