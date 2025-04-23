
badge_coverage <- function(x) {

  if (x >= 90.0) {
    uri_colour <- "blue"
  } else if (x < 90.0 & x >= 70.0) {
    uri_colour <- "yellowgreen"
  } else if (x < 70.0 & x >= 50.0) {
    uri_colour <- "orange"
  } else{
    uri_colour <- "red"
  }
  URLencode(sprintf("coverage-%.1f%%-%s.svg", x, uri_colour))
}

readme_path <- function(pkg = ".", use_rmd = TRUE) {
  pkg <- normalizePath(pkg)

  file_readme <- list.files(pkg, pattern = "README", full.names = TRUE)

  if (length(file_readme) == 0) {
    return(FALSE)
  }

  if (use_rmd) {
    use_rmd <- any(grepl("README.RMD$", file_readme, ignore.case = TRUE))
  }

  ext <- ifelse(use_rmd, "RMD", "MD")

  file_readme[grep(sprintf("README.%s", ext), toupper(file_readme))]

}

.check_badge <- function(path) {

  if (isFALSE(path)) {
    return(FALSE)
  }

  README_LINES <- readLines(path, warn = FALSE)

  any(grepl("badge/coverage-", README_LINES))
}


update_badge <- function(total_coverage, use_rmd = TRUE, update_rmd = FALSE){

  stopifnot(is.numeric(total_coverage))

  fpath <- readme_path(use_rmd = use_rmd)

  if (.check_badge(fpath)) {

    README_con <- readLines(fpath)

    idx_badge_line <- grepl("badge/coverage-", README_con)

    new_badge <- paste0('[![coverage](https://img.shields.io/badge/',
                        badge_coverage(total_coverage), ")](#)")
    README_con[idx_badge_line] <- new_badge

    cat(README_con, sep = "\n", file = fpath)

    if (update_rmd & grepl("RMD$", fpath, ignore.case = TRUE)) {
        rmarkdown::render(
          input = "README.Rmd",
          output_format = "github_document",
          output_file = "README.md",
          quiet = FALSE
        )
        Sys.sleep(1)
        unlink("README.html")
      }
  }
}

add_covr_txt <- function(cp){
  sink("data-coverage/R6.tiledb-coverage.txt")
  cat("# Test Coverage --------------------", sep = "\n")
  cat(paste0("Timestamp: ", date()), sep = "\n\n")
  cat(capture.output(cp, type = "message", append = TRUE), sep = "\n")
  sink()
}
