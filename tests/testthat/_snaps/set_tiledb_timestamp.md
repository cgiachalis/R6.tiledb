# 'print()' method for tiledb timestamps

    Code
      set_tiledb_timestamp(end_time = "1990-01-01", tz = "UTC")
    Output
      TileDB Timestamp (UTC〡user)
       * start: 1970-01-01 00:00:00
       * end  : 1990-01-01 00:00:00

---

    Code
      set_tiledb_timestamp(0, 1, tz = "Europe/London")
    Output
      TileDB Timestamp (Europe/London〡user)
       * start: 1970-01-01 01:00:00
       * end  : 1970-01-01 01:00:01

---

    Code
      print(set_tiledb_timestamp(0, 1, tz = "UTC"))
    Output
      TileDB Timestamp (UTC〡user)
       * start: 1970-01-01 00:00:00
       * end  : 1970-01-01 00:00:01

