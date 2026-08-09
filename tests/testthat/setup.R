# Force garbage collection after each test
withr::defer_parent(gc())
options(Ncpus = 1)
