
.isArray <- function(arr){
  methods::is(arr, "tiledb_sparse") || methods::is(arr, "tiledb_dense") || methods::is(arr, "tiledb_array")
}


