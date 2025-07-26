
path <- paste0(here::here(), "/data-coverage")
filepath <- paste0(path, "/covr-data.rds")

cli::cli_alert_info("Running covr...")

cp <- covr::package_coverage()
# cp <- covr::package_coverage(pre_clean = FALSE, clean = FALSE)

# cp <- readRDS(file = filepath)

cli::cli_alert_info("Running coverage report...")

pkgname <- read.dcf("DESCRIPTION")[[1, "Package"]]
covr::report(cp, file = file.path(path, paste0(pkgname, "-report.html")))


cli::cli_alert_info("Saving total coverage data...")

saveRDS(cp, file = filepath)

cli::cli_alert_info("Updating coverage badge in README...")

source(paste0(path, "/update_badge.R"))

total_coverage <- covr::coverage_to_list(cp)$totalcoverage
update_badge(total_coverage, update_rmd = FALSE)

add_covr_txt(cp)

