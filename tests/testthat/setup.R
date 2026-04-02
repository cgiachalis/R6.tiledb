# Force garbage collection after each test
withr::defer_parent(gc())
