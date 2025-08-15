# 'print()' method for tiledb timestamps

    Code
      set_tiledb_timestamp(end_time = "1990-01-01")
    Output
      TileDB Timestamp (user)
       * start: 1970-01-01 00:00:00
       * end  : 1990-01-01

---

    Code
      print(set_tiledb_timestamp(0, 1), tz = "Europe/London")
    Output
      TileDB Timestamp (user)
       * start: 1970-01-01 01:00:00
       * end  : 1970-01-01 01:00:01

---

    Code
      print(set_tiledb_timestamp(0, 1), tz = "UTC")
    Output
      TileDB Timestamp (user)
       * start: 1970-01-01 00:00:00
       * end  : 1970-01-01 00:00:01

