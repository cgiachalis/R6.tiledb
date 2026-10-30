# Force garbage collection after each test
withr::defer_parent(gc())
Sys.setenv(TESTTHAT_PARALLEL = "true")
Sys.setenv(TESTTHAT_CPUS=4)
